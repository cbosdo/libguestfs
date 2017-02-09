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
 * (1) Ignore the libosinfo library itself, since we don't care
 * for GObject nonsense.  The XML database contains all we need.
 *
 * (2) Ignore os/upgrades and os/derives-from fields.  This is
 * safe(-ish) since the media identifiers always change for every
 * release of an OS.  We can easily add support for this if it becomes
 * necessary.
 */

#include <config.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <dirent.h>
#include <errno.h>
#include <assert.h>
#include <sys/types.h>
#include <libintl.h>
#include <sys/stat.h>

#include "ignore-value.h"
#include "c-ctype.h"

#include "guestfs.h"
#include "guestfs-internal.h"

#include "osinfo.h"


/* Read the libosinfo XML database files.  The lock is held while
 * this is called.
 *
 * Returns:
 *   -1 => a fatal error ('error' has been called)
 *    0 => OK
 *
 * Note that failure to find or parse the XML files is *not* a fatal
 * error, since we should fall back silently if these are not
 * available.  Although we'll emit some debug if this happens.
 *
 * Try to use the shared osinfo database layout (and location) first:
 * https://gitlab.com/libosinfo/libosinfo/blob/master/docs/database-layout.txt
 */
static int read_osinfo_db_flat (guestfs_h *g, const char *directory, read_osinfo_db_callback callback, void *opaque);
static int read_osinfo_db_three_levels (guestfs_h *g, const char *directory, read_osinfo_db_callback callback, void *opaque);
static int read_osinfo_db_directory (guestfs_h *g, const char *directory, read_osinfo_db_callback callback, void *opaque);

int
read_osinfo_db (guestfs_h *g,
                read_osinfo_db_callback callback, void *opaque)
{
  int r;

  /* (1) Try the shared osinfo directory, using either the
   * $OSINFO_SYSTEM_DIR envvar or its default value.
   */
  {
    const char *path;
    CLEANUP_FREE char *os_path = NULL;

    path = getenv ("OSINFO_SYSTEM_DIR");
    if (path == NULL)
      path = "/usr/share/osinfo";
    os_path = safe_asprintf (g, "%s/os", path);
    r = read_osinfo_db_three_levels (g, os_path, callback, opaque);
  }
  if (r == -1)
    return -1;
  else if (r == 1)
    return 0;

  /* (2) Try the libosinfo directory, using the newer three-directory
   * layout ($LIBOSINFO_DB_PATH / "os" / $group-ID / [file.xml]).
   */
  r = read_osinfo_db_three_levels (g, LIBOSINFO_DB_PATH "/os", callback, opaque);
  if (r == -1)
    return -1;
  else if (r == 1)
    return 0;

  /* (3) Try the libosinfo directory, using the old flat directory
   * layout ($LIBOSINFO_DB_PATH / "oses" / [file.xml]).
   */
  r = read_osinfo_db_flat (g, LIBOSINFO_DB_PATH "/oses", callback, opaque);
  if (r == -1)
    return -1;
  else if (r == 1)
    return 0;

  /* Nothing found. */
  return 0;
}

static int
read_osinfo_db_flat (guestfs_h *g, const char *directory,
                     read_osinfo_db_callback callback, void *opaque)
{
  debug (g, "osinfo: loading flat database from %s", directory);

  return read_osinfo_db_directory (g, directory, callback, opaque);
}

static int
read_osinfo_db_three_levels (guestfs_h *g, const char *directory,
                             read_osinfo_db_callback callback, void *opaque)
{
  DIR *dir;
  int r;

  dir = opendir (directory);
  if (!dir) {
    debug (g, "osinfo: %s: %s", directory, strerror (errno));
    return 0; /* This is not an error: RHBZ#948324. */
  }

  debug (g, "osinfo: loading 3-level-directories database from %s", directory);

  for (;;) {
    struct dirent *d;
    CLEANUP_FREE char *pathname = NULL;
    struct stat sb;

    errno = 0;
    d = readdir (dir);
    if (!d) break;

    pathname = safe_asprintf (g, "%s/%s", directory, d->d_name);

    /* Iterate only on directories. */
    if (stat (pathname, &sb) == 0 && S_ISDIR (sb.st_mode)) {
      r = read_osinfo_db_directory (g, pathname, callback, opaque);
      if (r == -1)
        goto error;
    }
  }

  /* Check for failure in readdir. */
  if (errno != 0) {
    perrorf (g, "readdir: %s", directory);
    goto error;
  }

  /* Close the directory handle. */
  r = closedir (dir);
  dir = NULL;
  if (r == -1) {
    perrorf (g, "closedir: %s", directory);
    goto error;
  }

  return 1;

 error:
  if (dir)
    closedir (dir);

  return -1;
}

static int
read_osinfo_db_directory (guestfs_h *g, const char *directory,
                          read_osinfo_db_callback callback, void *opaque)
{
  DIR *dir;
  int r;

  dir = opendir (directory);
  if (!dir) {
    debug (g, "osinfo: %s: %s", directory, strerror (errno));
    return 0; /* This is not an error: RHBZ#948324. */
  }

  for (;;) {
    struct dirent *d;

    errno = 0;
    d = readdir (dir);
    if (!d) break;

    if (STRSUFFIX (d->d_name, ".xml")) {
      CLEANUP_FREE char *pathname = NULL;

      pathname = safe_asprintf (g, "%s/%s", directory, d->d_name);
      r = callback (g, pathname, opaque);
      if (r == -1)
        goto error;
    }
  }

  /* Check for failure in readdir. */
  if (errno != 0) {
    perrorf (g, "readdir: %s", directory);
    goto error;
  }

  /* Close the directory handle. */
  r = closedir (dir);
  dir = NULL;
  if (r == -1) {
    perrorf (g, "closedir: %s", directory);
    goto error;
  }

  return 1;

 error:
  if (dir)
    closedir (dir);

  return -1;
}
