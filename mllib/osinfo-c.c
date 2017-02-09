/* Bindings for osinfo db reading function.
 * Copyright (C) 2016 Red Hat Inc.
 * Copyright (C) 2017 SUSE Inc.
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
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */
#include <config.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <assert.h>

#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#include "guestfs.h"
#include "guestfs-internal.h"
#include "osinfo.h"

#pragma GCC diagnostic ignored "-Wmissing-prototypes"

struct callback_wrapper_args {
  /* In both case we are pointing to local roots, hence why these are
   * value* not value.
   */
  value *exnp;                  /* Safe place to store any exception
                                   raised by callback */
  value *fvp;                   /* callback. */
};

static int read_osinfo_db_callback_wrapper (guestfs_h *g, const char *path, void *opaque);

value
guestfs_int_mllib_read_osinfo_db (value gv, value fv)
{
  CAMLparam2 (gv, fv);
  guestfs_h *g = (guestfs_h *) Int64_val (gv);
  struct callback_wrapper_args args;

  /* This stack address is used to point to the exception, if one is
   * raised in the visitor_function.  Note that the macro initializes
   * this to Val_unit, which is how we know if an exception was set.
   */
  CAMLlocal1 (exn);

  exn = Val_unit;

  args.exnp = &exn;
  args.fvp = &fv;

  if (read_osinfo_db (g, read_osinfo_db_callback_wrapper, &args) == -1) {
    if (exn != Val_unit) {
      /* The failure was caused by the callback raising an
       * exception.  Re-raise it here.
       */
      caml_raise (exn);
    }

    caml_failwith ("read_osinfo_db");
}

  CAMLreturn (Val_unit);
}

static int
read_osinfo_db_callback_wrapper (guestfs_h *g, const char *path, void *opaque)
{
  CAMLparam0 ();
  CAMLlocal2 (pathv, v);
  struct callback_wrapper_args *args = opaque;

  assert (path != NULL);
  assert (args != NULL);

  pathv = caml_copy_string (path);

  v = caml_callback_exn (*args->fvp, pathv);

  if (Is_exception_result (v)) {
    *args->exnp = Extract_exception (v);
    CAMLreturnT (int, -1);
  }

  /* No error, return normally. */
  CAMLreturnT (int, 0);
}
