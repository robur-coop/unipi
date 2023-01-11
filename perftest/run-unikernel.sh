#! /bin/bash

set -e

SOLO5=/home/rand/.opam/mirage-tlstunnel/bin/solo5-hvt
#> Note: keep these in sync with test and init scripts
TAP_NAME=unipi_tap
IP=10.0.0.2/24
PORT=8888
OUT=output
#> Note:
# * daemon is listening on all interfaces, so is on 10.0.0.1 too
# * repo-name is defined in init.sh
# * branch need to be passed to unipi
REMOTE=git://10.0.0.1/unipi_web.git#main
MEM=512 #MB

if [ ! -e "$OUT" ]; then
    mkdir "$OUT"
fi

echo "$MEM" > "$OUT"/unipi_mem.txt

$SOLO5 --net:service="$TAP_NAME" --mem="$MEM" unipi.hvt \
          --ipv4 "$IP" --port "$PORT" \
          --remote "$REMOTE" \
          2>&1 > "$OUT"/unipi_stdout.txt &

echo $! > run-unikernel.sh.PID



