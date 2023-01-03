#! /bin/bash

set -e

ROOT="$PWD"
DATA_DIR=test-data
GIT_REPO_NAME=unipi_web
GIT_DAEMON_ROOT=git_daemon
GIT_DAEMON_DIR="$GIT_DAEMON_ROOT"/"$GIT_REPO_NAME"
GIT_CLIENT_DIR=git_client
SERVICE=perf_service

#// Networking

sudo ip link add "$SERVICE" type bridge
sudo ip addr add 10.0.0.1/24 dev "$SERVICE"
sudo ip link set dev "$SERVICE" up
sudo ip tuntap add tap100 mode tap
sudo ip link set dev tap100 up
sudo ip link set tap100 master "$SERVICE"

#// Git repo with files to serve

mkdir -p "$GIT_DAEMON_DIR"
cd "$GIT_DAEMON_DIR"
git init --bare
cd "$ROOT"

git daemon --reuseaddr --enable=receive-pack --base-path="$GIT_DAEMON_ROOT" "$GIT_DAEMON_ROOT" &
GIT_DAEMON_PID=$!

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





