#!/usr/bin/env bash

set -o errexit -o nounset -o pipefail

start_server() {
    bin/hsdns-server > /dev/null &
    server_pid="$!"
    trap 'kill "$server_pid"' EXIT
    sleep 0.1
}

dig_test() {
    local name="$1"
    local dig_cmd="$2"
    local expect="$3"
    echo -n "testing $name... "

    local answer
    answer="$(dig @localhost -p 1053 $dig_cmd)"
    # dig exits with 0 if it fails to connect.
    # anyway, the check of $? is pointless,
    # since this program exits if dig returns non-zero
    # (due to errexit).
    # it is being left here to revisit later.
    if [[ $? -eq 0 ]] && [[ "$answer" = "$expect" ]]; then
        echo ok.
    else
        echo failed.
    fi
}

main() {
    start_server

    dig_test "1 question" "+short +noedns danso.ca" "6.6.6.6"
    dig_test "no questions" "+short +noedns +header-only" ""
}

main
