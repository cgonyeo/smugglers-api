#/usr/bin/env bash

set -e
set -x

if [ "$EUID" -ne 0 ]; then
    echo "This script uses functionality which requires root privileges"
    exit 1
fi

if [ -e out ]; then
    rm -rf out
fi

nix-env -i smugglers-api -f . -p out

acbuildEnd() {
    export EXIT=$?
    acbuild --debug end && exit $EXIT 
}
trap acbuildEnd EXIT

# Start the build with an empty ACI
acbuild --debug begin

#for f in $(find out/ | tail -n +2); do acbuild copy-to-dir $(readlink -f $f)/* /$(basename $f); done
acbuild copy out/bin/smugglers-api /bin/smugglers-api

for i in $(ldd out/bin/smugglers-api | cut -d ' ' -f 3); do acbuild copy $i /lib/$(basename $i); done
for i in $(ldd out/bin/smugglers-api | cut -d ' ' -f 3); do f=$(readlink -f $i); acbuild copy $f /lib/$(basename $f); done
linkersym=$(ldd out/bin/smugglers-api | grep ld-linux | cut -d ' ' -f 1) 
linker=$(readlink -f $linkersym)
acbuild copy $linkersym $linkersym
acbuild copy $linker $linker

acbuild set-name smuggle.rs/api

acbuild set-exec /bin/smugglers-api

acbuild env add LD_LIBRARY_PATH /lib

acbuild write --overwrite smugglers-api.aci
