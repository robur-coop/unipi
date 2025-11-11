#! /bin/bash

set -e

prog_NAME=$(basename "${0}")
SCRIPT_ROOT=$(readlink -m $(dirname "${0}"))

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

usage () {
    cat <<EOM 1>&2
usage: ${prog_NAME} [ OPTIONS ]

Runs a performance-test of 'unipi', using the HTTP-server testing tool 'siege'.
The following steps happens:

* (non-unix target) Sets up a bridge to connect the TAP-device of 'unipi' to.
* Starts a 'git-daemon' that serves html to 'unipi'
* Starts 'unipi'
* Runs a 'siege' against the HTTP-server of 'unipi'
* (non-unix target) Tears down the network

The outputs of the test can be found in the 'test-output' directory.

Options:
    --target=STRING
        One of hvt, spt or unix. Defaults to hvt.
    --help
EOM
}

TARGET=hvt

while [ $# -gt 0 ]; do
    OPT="$1"

    case "${OPT}" in
        --target=*)
            TARGET="${OPT##*=}"
            ;;
        --help)
            usage
            exit 0
            ;;
        *)
            die "Unknown option: '${OPT}'"
            ;;
    esac
    shift
done

TAP_NAME=unipiperftap
SERVICE=unipiperf
SERVICE_IP=10.0.0.1

UNIPI_IP=10.0.0.2
UNIPI_PORT=8888

TARGET_EXT=""
case "$TARGET" in
    hvt)
        TARGET_EXT=."$TARGET"
        ;;
    spt)
        TARGET_EXT=."$TARGET"
        ;;
    unix)
        TARGET_EXT=""
        SERVICE_IP="127.0.0.1"
        UNIPI_IP="127.0.0.1"
        ;;
    *)
        die "Unknown target: '${TARGET}'"
        ;;
esac

cd "$SCRIPT_ROOT"

DATA_DIR=test-input
WORK_DIR=/tmp/test-workspace
if [ ! -e "$WORK_DIR" ]; then
    mkdir "$WORK_DIR"
fi
TEST_DIR=test-output
if [ ! -e "$TEST_DIR" ]; then
    mkdir "$TEST_DIR"
else
    rm -rf "$TEST_DIR"/*
fi

GIT_REPO_NAME=unipi_web.git
GIT_DAEMON_PORT=6543

#> Note: for local use where solo5 binaries are in opam, and it's builder-web that runs scripts
SOLO5_DIR=$(dirname $(which solo5-hvt))

cleanup () {
    info "killing unikernel"
    kill $(cat "$WORK_DIR"/run-unikernel.sh.PID 2>/dev/null) 2>/dev/null || \
        info ".. unikernel not running"

    info "killing git daemon"
    kill $(cat "$WORK_DIR"/init.sh.PID 2>/dev/null) 2>/dev/null || \
        info ".. git daemon not running"

    info "cleaning up work directory"
    rm -rf "$WORK_DIR"

    if [ "$TARGET" != "unix" ]; then
        cp teardown-network.sh /tmp/
        sudo /tmp/teardown-network.sh "$TAP_NAME" "$SERVICE"
        rm /tmp/teardown-network.sh
    fi
}

trap cleanup EXIT

cp ../dist/unipi"$TARGET_EXT" "$WORK_DIR"/ 2>/dev/null || \
    die "Didn't find dist/unipi$TARGET_EXT - you need to build the unikernel target first"

if [ "$TARGET" != "unix" ]; then
    info "initializing network"
    #> Note: 'cp to /tmp' to support sudo when using an encfs encrypted filesystem
    cp init-network.sh /tmp/
    sudo /tmp/init-network.sh "$TAP_NAME" "$SERVICE" "$SERVICE_IP"
    rm /tmp/init-network.sh
fi

info "initializing context for unikernel"
./init.sh \
    "$DATA_DIR" "$WORK_DIR" "$TEST_DIR" \
    "$GIT_REPO_NAME" "$GIT_DAEMON_PORT" &

info "sleeping before starting unipi"
sleep 5

info "checking if git daemon is still running"
kill -0 $(cat "$WORK_DIR"/init.sh.PID)

info "running unikernel in background"
./run-unikernel.sh \
    "$SOLO5_DIR" \
    "$SERVICE_IP" "$TAP_NAME" "$UNIPI_IP" "$UNIPI_PORT" \
    "$GIT_DAEMON_PORT" "$GIT_REPO_NAME" \
    "$WORK_DIR" "$TEST_DIR" \
    "$TARGET" "$TARGET_EXT" & 1>&2

info "sleeping a bit before test"
sleep 5

info "checking if unikernel is still running"
kill -0 $(cat "$WORK_DIR"/run-unikernel.sh.PID)

./run-test.sh "$UNIPI_IP" "$UNIPI_PORT" "$TEST_DIR"

info "successfully run test"


