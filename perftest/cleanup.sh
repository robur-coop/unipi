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

#> goto pass these, so scripts don't duplicate?
TAP_NAME=unipitap
SERVICE=unipiperf

info taking down networking
echo DRYRUN: needs superuser privileges
# ip link set dev "$TAP_NAME" down
# ip link del "$TAP_NAME"
# ip link set "$SERVICE" down
# ip link delete "$SERVICE" type bridge

info removing all files
rm -rf *

