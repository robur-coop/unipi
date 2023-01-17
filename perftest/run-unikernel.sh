#! /bin/bash

set -e

prog_NAME=$(basename "${0}")

warn()
{
    echo "${prog_NAME}: WARN: $*"
}

info()
{
    echo "${prog_NAME}: INFO: $*"
}

err()
{
    echo "${prog_NAME}: ERROR: $*" 1>&2
}

die()
{
    echo "${prog_NAME}: ERROR: $*" 1>&2
    exit 1
}

# >>> CONFIGURE PER SYSTEM
SOLO5=/home/rand/.opam/mirage-tlstunnel/bin/solo5-hvt
# <<< --------------------

#> Note: keep these in sync with test and init scripts
TAP_NAME=unipitap
IP=10.0.0.2/24
PORT=8888
OUT=output
#> Note: port, repo-name, git branch-name need to be in sync with init.sh 
GIT_DAEMON_PORT=6543
REMOTE=git://10.0.0.1:"$GIT_DAEMON_PORT"/unipi_web.git#main
#> 128 too little
MEM=512 #MB

if [ ! -e "$OUT" ]; then
    mkdir "$OUT"
fi

echo "$MEM" > "$OUT"/unipi_mem.txt

$SOLO5 --net:service="$TAP_NAME" --mem="$MEM" unipi.hvt \
          --ipv4 "$IP" --port "$PORT" \
          --remote "$REMOTE" \
          >"$OUT"/unipi_stdout.txt 2>&1 &

PID=$!
echo "$PID" > "$prog_NAME".PID



