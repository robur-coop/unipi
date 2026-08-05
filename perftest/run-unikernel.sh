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

#goto pass this many params as named args instead

SOLO5_DIR="$1"
[ -z "${SOLO5_DIR}" ] && die "<SOLO5_DIR> must be specified"
SERVICE_IP="$2"
[ -z "${SERVICE_IP}" ] && die "<SERVICE_IP> must be specified"
TAP_NAME="$3"
[ -z "${TAP_NAME}" ] && die "<TAP_NAME> must be specified"
IP="$4"
[ -z "${IP}" ] && die "<IP> must be specified"
IP="$IP"/24
PORT="$5"
[ -z "${PORT}" ] && die "<PORT> must be specified"
GIT_DAEMON_PORT="$6"
[ -z "${GIT_DAEMON_PORT}" ] && die "<GIT_DAEMON_PORT> must be specified"
GIT_REPO_NAME="$7"
[ -z "${GIT_REPO_NAME}" ] && die "<GIT_REPO_NAME> must be specified"

WORK_DIR="$8"
[ -z "${WORK_DIR}" ] && die "<WORK_DIR> must be specified"
if [ ! -e "$WORK_DIR" ]; then
    mkdir "$WORK_DIR"
fi
TEST_DIR="$9"
[ -z "${TEST_DIR}" ] && die "<TEST_DIR> must be specified"
if [ ! -e "$TEST_DIR" ]; then
    mkdir "$TEST_DIR"
fi
info "DEBUG: work-dir = $WORK_DIR"
info "DEBUG: test-dir = $TEST_DIR"

TARGET="${10}"
TARGET_EXT="${11}"

SOLO5="$SOLO5_DIR"/solo5-"$TARGET"

REMOTE='git://'"$SERVICE_IP":"$GIT_DAEMON_PORT"/"$GIT_REPO_NAME"'#main'
info "DEBUG: remote = '$REMOTE'"

#> 128 too little
MEM=512 #MB

echo "$MEM" > "$TEST_DIR"/unipi_mem.txt

touch "$TEST_DIR"/unipi_stdout.txt
tail -f "$TEST_DIR"/unipi_stdout.txt | grep -v '\[application\] requested' &
if [ "$TARGET" = "unix" ]; then
    "$WORK_DIR"/unipi \
               --port "$PORT" \
               --remote "$REMOTE" \
               > "$TEST_DIR"/unipi_stdout.txt 2>&1 &
else 
    $SOLO5 --net:service="$TAP_NAME" --mem="$MEM" "$WORK_DIR"/unipi"$TARGET_EXT" \
           --ipv4 "$IP" --port "$PORT" \
           --remote "$REMOTE" \
           > "$TEST_DIR"/unipi_stdout.txt 2>&1 &
fi

PID=$!
echo "$PID" > "$WORK_DIR"/"$prog_NAME".PID



