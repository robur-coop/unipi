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
SERVICE_IP="$3"
[ -z "${SERVICE_IP}" ] && die "<SERVICE_IP> must be specified"

if ip link | grep -q "$TAP_NAME" && ip link | grep -q "$SERVICE"; then
    info "network already set up, doing nothing"
else 
    info "setting up networking"
    #> Note: need superuser privileges
    ip link add "$SERVICE" type bridge
    ip addr add "$SERVICE_IP"/24 dev "$SERVICE"
    ip link set dev "$SERVICE" up
    ip tuntap add "$TAP_NAME" mode tap
    ip link set dev "$TAP_NAME" up
    ip link set "$TAP_NAME" master "$SERVICE"
fi
