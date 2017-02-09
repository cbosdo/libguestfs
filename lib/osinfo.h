/* libguestfs
 * Copyright (C) 2017 Red Hat Inc.
 * Copyright (C) 2017 SUSE Inc.
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

#ifndef OSINFO_H
#define OSINFO_H

typedef int (*read_osinfo_db_callback) (guestfs_h *g, const char *path, void *opaque);

extern int read_osinfo_db (guestfs_h *g, read_osinfo_db_callback callback, void *opaque);

#endif /* OSINFO_H */
