#! /bin/bash

set -e

#> Note: keep these in sync with test and init scripts
TAP_NAME=unipi_tap
IP=10.0.0.2/24
PORT=8888
OUT=output
#> Note:
# * daemon is listening on all interfaces, so is on 10.0.0.1 too
# * repo-name is defined in init.sh
# * branch need to be passed to unipi, and is gits default
REMOTE=git://10.0.0.1/unipi_web#master
MEM=512 #MB

if [ ! -e "$OUT" ]; then
    mkdir "$OUT"
fi

echo "$MEM" > "$OUT"/unipi_mem.txt

solo5-hvt --net:service="$TAP_NAME" --mem="$MEM" unipi.hvt \
          --ipv4 "$IP" --port "$PORT" \
          --remote "$REMOTE" \
          2>&1 > "$OUT"/unipi_stdout.txt



