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

TAP_NAME="$1"
[ -z "${TAP_NAME}" ] && die "<TAP_NAME> must be specified"
SERVICE="$2"
[ -z "${SERVICE}" ] && die "<SERVICE> must be specified"

info "taking down networking"
ip link set dev "$TAP_NAME" down
ip link del "$TAP_NAME"
ip link set "$SERVICE" down
ip link delete "$SERVICE" type bridge



