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

UNIPI_IP="$1"
[ -z "${UNIPI_IP}" ] && die "<UNIPI_IP> must be specified"
UNIPI_PORT="$2"
[ -z "${UNIPI_PORT}" ] && die "<UNIPI_PORT> must be specified"
UNIPI="http://${UNIPI_IP}:${UNIPI_PORT}"
TEST_DIR="$3"
[ -z "${TEST_DIR}" ] && die "<TEST_DIR> must be specified"
if [ ! -e "$TEST_DIR" ]; then
    mkdir "$TEST_DIR"
fi

TEST_TIME=20S
# TEST_TIME=2S

uname -a > "$TEST_DIR"/platform.txt
date > "$TEST_DIR"/time.txt

if [ "$(uname)" = "FreeBSD" ]; then
    sysctl hw.model hw.machine hw.ncpu > "$TEST_DIR"/cpu.txt
elif [ "$(uname)" = "Linux" ]; then
    cat /proc/cpuinfo > "$TEST_DIR"/cpu.txt
else
    die unsupported platform
fi

siege --version > "$TEST_DIR"/siege_version.txt

TEST_NAME=siege_test01
siege --concurrent=30 -t"$TEST_TIME" -b --log="$TEST_DIR/$TEST_NAME".csv --no-parser \
      "$UNIPI"/index.html \
      >"$TEST_DIR/$TEST_NAME".stdout 2>&1 




