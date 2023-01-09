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
TAP_NAME=unipi_tap
SERVICE=unipi_perf_service

info taking down networking
sudo ip link set dev "$TAP_NAME" down
sudo ip link del "$TAP_NAME"
sudo ip link set "$SERVICE" down
sudo ip link delete "$SERVICE" type bridge

info removing all files
#> goto how to delete non-existent hidden files without error?
# .. change what wild-card expands to?
rm -rf * .*

