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

ROOT="$PWD"
DATA_DIR=test-data
TAP_NAME=unipi_tap
SERVICE=unipi_perf_service
GIT_REPO_NAME=unipi_web
GIT_DAEMON_ROOT=git_daemon
GIT_DAEMON_DIR="$GIT_DAEMON_ROOT"/"$GIT_REPO_NAME"
GIT_CLIENT_DIR=git_client

#// Networking

info setting up networking
sudo ip link add "$SERVICE" type bridge
sudo ip addr add 10.0.0.1/24 dev "$SERVICE"
sudo ip link set dev "$SERVICE" up
sudo ip tuntap add "$TAP_NAME" mode tap
sudo ip link set dev "$TAP_NAME" up
sudo ip link set "$TAP_NAME" master "$SERVICE"

#// Git repo with files to serve

info setting up git daemon dir
mkdir -p "$GIT_DAEMON_DIR"
cd "$GIT_DAEMON_DIR"
git init --bare
cd "$ROOT"

info running git daemon in bg
git daemon --reuseaddr --enable=receive-pack --base-path="$GIT_DAEMON_ROOT" "$GIT_DAEMON_ROOT" &
GIT_DAEMON_PID=$!

info adding index.html to git repo
mkdir "$GIT_CLIENT_DIR"
cd "$GIT_CLIENT_DIR"
git remote add origin git://127.0.0.1/"$GIT_REPO_NAME"
cp "$ROOT"/"$DATA_DIR"/index.html ./
git add index.html
git commit -m "index.html"
git push
cd "$ROOT"

#// Foregrounding the daemon, so ssh script can kill it when unipi test is done
# .. maybe not needed, as ssh doesn't seem to exit unless all processes (bg too) are done?
fg 





