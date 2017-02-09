/* libguestfs
 * Copyright (C) 2012 Red Hat Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */

/* Read libosinfo XML files to parse out just the
 * os/media/iso/system-id and os/media/iso/volume-id fields, which we
 * can then use to map install media to operating systems.
 *
 * Note some assumptions here:
 *
 * (1) We have to do some translation of the distro names and versions
 * stored in the libosinfo files and the standard names returned by
 * libguestfs.
 *
 * (2) Media detection is only part of the story.  We may still need
 * to inspect inside the image.
 *
 * (3) We only read the XML database files (at most) once per process,
 * and keep them cached.  They are only read at all if someone tries
 * to inspect a CD/DVD/ISO.
 *
 * XXX Currently the database is not freed when the program exits /
 * library is unloaded, although we should probably do that.
 */

#include <config.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <dirent.h>
#include <assert.h>
#include <sys/types.h>
#include <libintl.h>
#include <sys/stat.h>

#include <libxml/parser.h>
#include <libxml/xpath.h>

#include "ignore-value.h"
#include "glthread/lock.h"
#include "c-ctype.h"

#include "guestfs.h"
#include "guestfs-internal.h"

#include "osinfo.h"

gl_lock_define_initialized (static, osinfo_db_lock);
static ssize_t osinfo_db_size = 0; /* 0 = unread, -1 = error, >= 1 = #records */
static struct osinfo *osinfo_db = NULL;

static void free_osinfo_db_entry (struct osinfo *);

#define XMLSTREQ(a,b) (xmlStrEqual((a),(b)) == 1)

static int read_osinfo_db_xml (guestfs_h *g, const char *pathname, void *data);

/* Given one or more fields from the header of a CD/DVD/ISO, look up
 * the media in the libosinfo database and return our best guess for
 * the operating system.
 *
 * This returns:
 *   -1 => a fatal error ('error' has been called, caller must not ignore it)
 *   0  => could not locate the OS
 *   1  => matching OS found, the osinfo_ret pointer has been filled in
 */
int
guestfs_int_osinfo_map (guestfs_h *g, const struct guestfs_isoinfo *isoinfo,
                        const struct osinfo **osinfo_ret)
{
  size_t i;

  /* We only need to lock the database when reading it for the first time. */
  gl_lock_lock (osinfo_db_lock);
  if (osinfo_db_size == 0) {
    if (read_osinfo_db (g, read_osinfo_db_xml, NULL) == -1) {
      /* Fatal error: free any database entries which have been read, and
       * mark the database as having a permanent error.
       */
      if (osinfo_db_size > 0) {
        for (i = 0; i < (size_t) osinfo_db_size; ++i)
          free_osinfo_db_entry (&osinfo_db[i]);
      }
      free (osinfo_db);
      osinfo_db = NULL;
      osinfo_db_size = -1;
      gl_lock_unlock (osinfo_db_lock);
      return -1;
    }
  }
  gl_lock_unlock (osinfo_db_lock);

  if (osinfo_db_size <= 0)
    return 0;

  /* Look in the database to see if we can find a match. */
  for (i = 0; i < (size_t) osinfo_db_size; ++i) {
    if (osinfo_db[i].re_system_id) {
      if (!isoinfo->iso_system_id ||
          !match (g, isoinfo->iso_system_id, osinfo_db[i].re_system_id))
        continue;
    }

    if (osinfo_db[i].re_volume_id) {
      if (!isoinfo->iso_volume_id ||
          !match (g, isoinfo->iso_volume_id, osinfo_db[i].re_volume_id))
        continue;
    }

    if (osinfo_db[i].re_publisher_id) {
      if (!isoinfo->iso_publisher_id ||
          !match (g, isoinfo->iso_publisher_id, osinfo_db[i].re_publisher_id))
        continue;
    }

    if (osinfo_db[i].re_application_id) {
      if (!isoinfo->iso_application_id ||
          !match (g, isoinfo->iso_application_id, osinfo_db[i].re_application_id))
        continue;
    }

    debug (g, "osinfo: mapped disk to database entry %zu", i);

    if (osinfo_ret)
      *osinfo_ret = &osinfo_db[i];
    return 1;
  }

  debug (g, "osinfo: no mapping found");

  return 0;
}

static int read_iso_node (guestfs_h *g, xmlNodePtr iso_node, struct osinfo *osinfo);
static int read_media_node (guestfs_h *g, xmlXPathContextPtr xpathCtx, xmlNodePtr media_node, struct osinfo *osinfo);
static int read_os_node (guestfs_h *g, xmlXPathContextPtr xpathCtx, xmlNodePtr os_node, struct osinfo *osinfo);

/* Read a single XML file from pathname (which is a full path).
 * Only memory allocation failures are fatal errors here.
 */
static int
read_osinfo_db_xml (guestfs_h *g, const char *pathname, void *opaque)
{
  CLEANUP_XMLFREEDOC xmlDocPtr doc = NULL;
  CLEANUP_XMLXPATHFREECONTEXT xmlXPathContextPtr xpathCtx = NULL;
  CLEANUP_XMLXPATHFREEOBJECT xmlXPathObjectPtr xpathObj = NULL;
  xmlNodeSetPtr nodes;
  xmlNodePtr iso_node, media_node, os_node;
  struct osinfo *osinfo;
  size_t i;

  doc = xmlReadFile (pathname, NULL, XML_PARSE_NONET);
  if (doc == NULL) {
    debug (g, "osinfo: unable to parse XML file %s", pathname);
    return 0;
  }

  xpathCtx = xmlXPathNewContext (doc);
  if (xpathCtx == NULL) {
    error (g, _("osinfo: unable to create new XPath context"));
    return -1;
  }

  /* Get all <iso> nodes at any depth, then use the parent pointers in
   * order to work back up the tree.
   */
  xpathObj = xmlXPathEvalExpression (BAD_CAST "/libosinfo/os/media/iso",
                                     xpathCtx);
  if (xpathObj == NULL) {
    error (g, _("osinfo: %s: unable to evaluate XPath expression"),
           pathname);
    return -1;
  }

  nodes = xpathObj->nodesetval;

  if (nodes != NULL) {
    for (i = 0; i < (size_t) nodes->nodeNr; ++i) {
      iso_node = nodes->nodeTab[i];
      assert (iso_node != NULL);
      assert (STREQ ((const char *) iso_node->name, "iso"));
      assert (iso_node->type == XML_ELEMENT_NODE);

      media_node = iso_node->parent;
      assert (media_node != NULL);
      assert (STREQ ((const char *) media_node->name, "media"));
      assert (media_node->type == XML_ELEMENT_NODE);

      os_node = media_node->parent;
      assert (os_node != NULL);
      assert (STREQ ((const char *) os_node->name, "os"));
      assert (os_node->type == XML_ELEMENT_NODE);

      /* Allocate an osinfo record. */
      osinfo_db_size++;
      osinfo_db = safe_realloc (g, osinfo_db,
                                sizeof (struct osinfo) * osinfo_db_size);
      osinfo = &osinfo_db[osinfo_db_size-1];
      memset (osinfo, 0, sizeof *osinfo);

      /* Read XML fields into the new osinfo record. */
      if (read_iso_node (g, iso_node, osinfo) == -1 ||
          read_media_node (g, xpathCtx, media_node, osinfo) == -1 ||
          read_os_node (g, xpathCtx, os_node, osinfo) == -1) {
        free_osinfo_db_entry (osinfo);
        osinfo_db_size--;
        return -1;
      }

#if 0
      debug (g, "osinfo: %s: %s%s%s%s=> arch %s live %s installer %s product %s type %d distro %d version %d.%d",
             pathname,
             osinfo->re_system_id ? "<system-id/> " : "",
             osinfo->re_volume_id ? "<volume-id/> " : "",
             osinfo->re_publisher_id ? "<publisher-id/> " : "",
             osinfo->re_application_id ? "<application-id/> " : "",
             osinfo->arch ? osinfo->arch : "(none)",
             osinfo->is_live_disk ? "true" : "false",
             osinfo->is_installer ? "true" : "false",
             osinfo->product_name ? osinfo->product_name : "(none)",
             (int) osinfo->type, (int) osinfo->distro,
             osinfo->major_version, osinfo->minor_version);
#endif
    }
  }

  return 0;
}

static int compile_re (guestfs_h *g, xmlNodePtr child, pcre **re);

/* Read the regular expressions under the <iso> node.  libosinfo
 * itself uses the glib function 'g_regex_match_simple'.  That appears
 * to implement PCRE, however I have not checked in detail.
 */
static int
read_iso_node (guestfs_h *g, xmlNodePtr iso_node, struct osinfo *osinfo)
{
  xmlNodePtr child;

  for (child = iso_node->children; child; child = child->next) {
    if (STREQ ((const char *) child->name, "system-id")) {
      if (compile_re (g, child, &osinfo->re_system_id) == -1)
        return -1;
    }
    else if (STREQ ((const char *) child->name, "volume-id")) {
      if (compile_re (g, child, &osinfo->re_volume_id) == -1)
        return -1;
    }
    else if (STREQ ((const char *) child->name, "publisher-id")) {
      if (compile_re (g, child, &osinfo->re_publisher_id) == -1)
        return -1;
    }
    else if (STREQ ((const char *) child->name, "application-id")) {
      if (compile_re (g, child, &osinfo->re_application_id) == -1)
        return -1;
    }
  }

  return 0;
}

static int
compile_re (guestfs_h *g, xmlNodePtr node, pcre **re)
{
  const char *err;
  int offset;
  CLEANUP_FREE char *content = (char *) xmlNodeGetContent (node);

  if (content) {
    *re = pcre_compile (content, 0, &err, &offset, NULL);
    if (*re == NULL)
      debug (g, "osinfo: could not parse regular expression '%s': %s (ignored)",
             content, err);
  }

  return 0;
}

/* Read the attributes of the <media/> node. */
static int
read_media_node (guestfs_h *g, xmlXPathContextPtr xpathCtx,
                 xmlNodePtr media_node, struct osinfo *osinfo)
{
  osinfo->arch = (char *) xmlGetProp (media_node, BAD_CAST "arch");

  osinfo->is_live_disk = 0; /* If no 'live' attr, defaults to false. */
  {
    CLEANUP_XMLFREE xmlChar *content = NULL;
    content = xmlGetProp (media_node, BAD_CAST "live");
    if (content)
      osinfo->is_live_disk = XMLSTREQ (content, BAD_CAST "true");
  }

  osinfo->is_installer = true; /* If no 'installer' attr, defaults to true. */
  {
    CLEANUP_XMLFREE xmlChar *content = NULL;
    content = xmlGetProp (media_node, BAD_CAST "installer");
    if (content)
      osinfo->is_installer = XMLSTREQ (content, BAD_CAST "true");
  }

  return 0;
}

static int parse_version (guestfs_h *g, xmlNodePtr node, struct osinfo *osinfo);
static int parse_family (guestfs_h *g, xmlNodePtr node, struct osinfo *osinfo);
static int parse_distro (guestfs_h *g, xmlNodePtr node, struct osinfo *osinfo);

/* Read some fields under the <os/> node. */
static int
read_os_node (guestfs_h *g, xmlXPathContextPtr xpathCtx,
              xmlNodePtr os_node, struct osinfo *osinfo)
{
  xmlNodePtr child;

  for (child = os_node->children; child; child = child->next) {
    if (STREQ ((const char *) child->name, "name"))
      osinfo->product_name = (char *) xmlNodeGetContent (child);
    else if (STREQ ((const char *) child->name, "version")) {
      if (parse_version (g, child, osinfo) == -1)
        return -1;
    }
    else if (STREQ ((const char *) child->name, "family")) {
      if (parse_family (g, child, osinfo) == -1)
        return -1;
    }
    else if (STREQ ((const char *) child->name, "distro")) {
      if (parse_distro (g, child, osinfo) == -1)
        return -1;
    }
  }

  return 0;
}

static int
parse_version (guestfs_h *g, xmlNodePtr node, struct osinfo *osinfo)
{
  CLEANUP_FREE char *content = NULL;

  content = (char *) xmlNodeGetContent (node);
  /* We parse either "X.Y" or "X" as version strings, so try to parse
   * only if the first character is a digit.
   */
  if (content && c_isdigit (content[0])) {
    struct version version;
    const int res = guestfs_int_version_from_x_y_or_x (g, &version, content);
    if (res < 0)
      return -1;
    else if (res > 0) {
      osinfo->major_version = version.v_major;
      osinfo->minor_version = version.v_minor;
    }
  }

  return 0;
}

static int
parse_family (guestfs_h *g, xmlNodePtr node, struct osinfo *osinfo)
{
  CLEANUP_FREE char *content = NULL;

  osinfo->type = OS_TYPE_UNKNOWN;

  content = (char *) xmlNodeGetContent (node);
  if (content) {
    if (STREQ (content, "linux"))
      osinfo->type = OS_TYPE_LINUX;
    else if (STRPREFIX (content, "win"))
      osinfo->type = OS_TYPE_WINDOWS;
    else if (STREQ (content, "freebsd"))
      osinfo->type = OS_TYPE_FREEBSD;
    else if (STREQ (content, "netbsd"))
      osinfo->type = OS_TYPE_NETBSD;
    else if (STREQ (content, "msdos"))
      osinfo->type = OS_TYPE_DOS;
    else if (STREQ (content, "openbsd"))
      osinfo->type = OS_TYPE_OPENBSD;
    else
      debug (g, "osinfo: warning: unknown <family> '%s'", content);
  }

  return 0;
}

static int
parse_distro (guestfs_h *g, xmlNodePtr node, struct osinfo *osinfo)
{
  CLEANUP_FREE char *content = NULL;

  osinfo->distro = OS_DISTRO_UNKNOWN;

  content = (char *) xmlNodeGetContent (node);
  if (content) {
    if (STREQ (content, "altlinux"))
      osinfo->distro = OS_DISTRO_ALTLINUX;
    else if (STREQ (content, "centos"))
      osinfo->distro = OS_DISTRO_CENTOS;
    else if (STREQ (content, "debian"))
      osinfo->distro = OS_DISTRO_DEBIAN;
    else if (STREQ (content, "fedora"))
      osinfo->distro = OS_DISTRO_FEDORA;
    else if (STREQ (content, "freebsd"))
      osinfo->distro = OS_DISTRO_FREEBSD;
    else if (STREQ (content, "mageia"))
      osinfo->distro = OS_DISTRO_MAGEIA;
    else if (STREQ (content, "mandriva"))
      osinfo->distro = OS_DISTRO_MANDRIVA;
    else if (STREQ (content, "netbsd"))
      osinfo->distro = OS_DISTRO_NETBSD;
    else if (STREQ (content, "openbsd"))
      osinfo->distro = OS_DISTRO_OPENBSD;
    else if (STREQ (content, "opensuse"))
      osinfo->distro = OS_DISTRO_OPENSUSE;
    else if (STREQ (content, "rhel"))
      osinfo->distro = OS_DISTRO_RHEL;
    else if (STREQ (content, "sled") || STREQ (content, "sles"))
      osinfo->distro = OS_DISTRO_SLES;
    else if (STREQ (content, "ubuntu"))
      osinfo->distro = OS_DISTRO_UBUNTU;
    else if (STRPREFIX (content, "win"))
      osinfo->distro = OS_DISTRO_WINDOWS;
    else
      debug (g, "osinfo: warning: unknown <distro> '%s'", content);
  }

  return 0;
}

static void
free_osinfo_db_entry (struct osinfo *osinfo)
{
  free (osinfo->product_name);
  free (osinfo->arch);

  if (osinfo->re_system_id)
    pcre_free (osinfo->re_system_id);
  if (osinfo->re_volume_id)
    pcre_free (osinfo->re_volume_id);
  if (osinfo->re_publisher_id)
    pcre_free (osinfo->re_publisher_id);
  if (osinfo->re_application_id)
    pcre_free (osinfo->re_application_id);
}
