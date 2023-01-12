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
TAP_NAME=unipitap
SERVICE=unipiperf
GIT_REPO_NAME=unipi_web.git
GIT_DAEMON_PORT=6543
GIT_DAEMON_ROOT=git_daemon
GIT_DAEMON_DIR="$GIT_DAEMON_ROOT"/"$GIT_REPO_NAME"
GIT_CLIENT_DIR=git_client

# >>> CONFIGURE PER SYSTEM
# goto choose special service subnet here, to not collide with existing,
# .. and keep in sync with other scripts
# <<< --------------------

#// Networking

#> Note: need superuser privileges

info setting up networking
echo DRYRUN: already setup networking, as demands super-user rights
# ip link add "$SERVICE" type bridge
# ip addr add 10.0.0.1/24 dev "$SERVICE"
# ip link set dev "$SERVICE" up
# ip tuntap add "$TAP_NAME" mode tap
# ip link set dev "$TAP_NAME" up
# ip link set "$TAP_NAME" master "$SERVICE"

#// Git repo with files to serve

info setting up git daemon dir
mkdir -p "$GIT_DAEMON_DIR"
cd "$GIT_DAEMON_DIR"
git init --bare
git branch -m master main
cp hooks/post-update.sample hooks/post-update
chmod a+x hooks/post-update
touch git-daemon-export-ok
cd "$ROOT"

info running git daemon in bg
git daemon --port="$GIT_DAEMON_PORT" --reuseaddr --enable=receive-pack --base-path="$GIT_DAEMON_ROOT" "$GIT_DAEMON_ROOT" &
echo $! > init.sh.PID

info adding index.html to git repo
#git init
#git remote add origin git://127.0.0.1/"$GIT_REPO_NAME"
git clone git://127.0.0.1:"$GIT_DAEMON_PORT"/"$GIT_REPO_NAME" "$GIT_CLIENT_DIR"
cd "$GIT_CLIENT_DIR"
#goto add a new ('unipi'?) branch if not existing (to not rely on git naming)
cp "$ROOT"/"$DATA_DIR"/index.html ./
git add index.html
git commit -m "index.html"
git push
cd "$ROOT"

#// Foregrounding the daemon, so ssh script can kill it when unipi test is done
# .. maybe not needed, as ssh doesn't seem to exit unless all processes (bg too) are done?
#fg 





