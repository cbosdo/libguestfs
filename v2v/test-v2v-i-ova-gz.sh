#!/bin/bash -
# libguestfs virt-v2v test script
# Copyright (C) 2014 Red Hat Inc.
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

# Test -i ova option with gzip-compressed disks.

unset CDPATH
export LANG=C
set -e

if [ -n "$SKIP_TEST_V2V_I_OVA_GZ_SH" ]; then
    echo "$0: test skipped because environment variable is set"
    exit 77
fi

if [ "$(guestfish get-backend)" = "uml" ]; then
    echo "$0: test skipped because UML backend does not support network"
    exit 77
fi

virt_tools_data_dir=${VIRT_TOOLS_DATA_DIR:-/usr/share/virt-tools}
if ! test -r $virt_tools_data_dir/rhsrvany.exe; then
    echo "$0: test skipped because rhsrvany.exe is not installed"
    exit 77
fi

d=test-v2v-i-ova-gz.d
rm -rf $d
mkdir $d

pushd $d

truncate -s 10k disk1.vmdk
gzip disk1.vmdk
sha=`sha1sum disk1.vmdk.gz | awk '{print $1}'`
echo -e "SHA1(disk1.vmdk.gz)=$sha\r" > disk1.mf

tar -cf test.ova ../test-v2v-i-ova-gz.ovf disk1.vmdk.gz disk1.mf
popd

# Run virt-v2v but only as far as the --print-source stage, and
# normalize the output.
$VG virt-v2v --debug-gc --quiet \
    -i ova $d/test.ova \
    --print-source |
sed 's,[^ \t]*\(\.vmdk\),\1,' > $d/source

# Check the parsed source is what we expect.
diff -u test-v2v-i-ova-gz.expected $d/source

rm -rf $d