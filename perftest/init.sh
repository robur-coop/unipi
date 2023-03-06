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

ROOT="$PWD"
DATA_DIR="$1"
[ -z "${DATA_DIR}" ] && die "<DATA_DIR> must be specified"
WORK_DIR="$2"
[ -z "${WORK_DIR}" ] && die "<WORK_DIR> must be specified"
TEST_DIR="$3"
[ -z "${TEST_DIR}" ] && die "<TEST_DIR> must be specified"

GIT_REPO_NAME="$4"
[ -z "${GIT_REPO_NAME}" ] && die "<GIT_REPO_NAME> must be specified"
GIT_DAEMON_PORT="$5"
[ -z "${GIT_DAEMON_PORT}" ] && die "<GIT_DAEMON_PORT> must be specified"
GIT_DAEMON_ROOT="$WORK_DIR"/git_daemon
GIT_DAEMON_DIR="$GIT_DAEMON_ROOT"/"$GIT_REPO_NAME"
GIT_CLIENT_DIR="$WORK_DIR"/git_client

#// Git repo with files to serve

info setting up git daemon dir
mkdir -p "$GIT_DAEMON_DIR"
cd "$GIT_DAEMON_DIR"
git init --bare
#> goto add 'main' as param to share?
git branch -m main
cp hooks/post-update.sample hooks/post-update
chmod a+x hooks/post-update
touch git-daemon-export-ok
cd "$ROOT"

info "running git daemon in background"
git daemon \
    --port="$GIT_DAEMON_PORT" \
    --reuseaddr \
    --enable=receive-pack \
    --base-path="$GIT_DAEMON_ROOT" \
    "$GIT_DAEMON_ROOT" &

PID=$!
echo "$PID" > "$WORK_DIR"/"$prog_NAME".PID

info sleeping a bit before cloning git repo served by git daemon
sleep 2

info adding index.html to git repo
#git init
#git remote add origin git://127.0.0.1/"$GIT_REPO_NAME"
git clone git://127.0.0.1:"$GIT_DAEMON_PORT"/"$GIT_REPO_NAME" "$GIT_CLIENT_DIR" \
    && info successfully cloned repo
cd "$GIT_CLIENT_DIR"
#goto add a new ('unipi'?) branch if not existing (to not rely on git naming)
cp "$ROOT"/"$DATA_DIR"/index.html ./
git add index.html
git commit -m "index.html"
git push
cd "$ROOT"






