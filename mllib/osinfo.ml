(* virt-builder
 * Copyright (C) 2016 - SUSE Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 *)
open Common_utils

type osinfo_db_callback = string -> unit

external c_read_osinfo_db : int64 -> osinfo_db_callback -> unit =
  "guestfs_int_mllib_read_osinfo_db"

let read_osinfo_db g f =
  c_read_osinfo_db (Guestfs.c_pointer g) f
