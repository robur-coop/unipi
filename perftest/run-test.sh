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

UNIPI_IP=10.0.0.2
UNIPI_PORT=8888
UNIPI="http://${UNIPI_IP}:${UNIPI_PORT}"
#TEST_TIME=20S
TEST_TIME=2S
OUT=output
if [ ! -e "$OUT" ]; then
    mkdir "$OUT"
fi

uname -a > "$OUT"/platform.txt
date > "$OUT"/time.txt

if [ $(uname) = "FreeBSD" ]; then
    sysctl hw.model hw.machine hw.ncpu > "$OUT"/cpu.txt
elif [ $(uname) = "Linux" ]; then
    cat /proc/cpuinfo > "$OUT"/cpu.txt
else
    die unsupported platform
fi

siege --version > "$OUT"/siege_version.txt

TESTNAME=siege_test01
siege --concurrent=30 -t"$TEST_TIME" -b --log="$OUT/$TESTNAME".csv --no-parser \
      "$UNIPI"/index.html \
      2>&1 1>"$OUT/$TESTNAME".stdout




