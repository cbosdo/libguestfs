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

(** Bindings for the lib/osinfo.h os-info database reading API. *)

type osinfo_db_callback = string -> unit
(** The osinfo_db_callback is a callback called for each data file
    in the os-info database. The argument of the function is
    the absolute path of the data file.

    The callback may raise an exception, which will cause the whole
    database read to fail with an error (raising the same exception). *)

val read_osinfo_db : Guestfs.t -> osinfo_db_callback -> unit
(** [read_osinfo_db g callback] will find all the os-info database
    files and call the callback on them. *)
