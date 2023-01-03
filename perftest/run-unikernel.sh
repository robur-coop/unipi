#! /bin/bash

set -e

#> Note: keep these in sync with test and init scripts
TAP_NAME=unipi_tap
IP=10.0.0.2/24
PORT=8888
OUT=output

if [ ! -e "$OUT" ]; then
    mkdir "$OUT"
fi

#goto save amount of memory assigned to unipi

solo5-hvt --net:service="$TAP_NAME" unipi.hvt \
          --ipv4 "$IP" --port "$PORT" \
          --remote git://10.0.0.1/unipi_web#master \
          2>&1 > "$OUT"/unipi.stdout



