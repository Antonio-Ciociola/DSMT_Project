#!/usr/bin/env bash

# Script to start auction node in Docker (non-interactive)

NODE_NAME="auction"
HTTP_PORT="8081"
COOKIE="auction_secret"
EBIN_DIR="ebin"

echo "=============================================="
echo "  Starting Auction Node in Docker"
echo "  Node: ${NODE_NAME}"
echo "  HTTP Port: ${HTTP_PORT}"
echo "=============================================="
echo ""

# Start Erlang node without interactive shell, keeping it running
exec erl -sname "$NODE_NAME" \
    -setcookie "$COOKIE" \
    -pa "$EBIN_DIR" \
    -pa _build/default/lib/cowboy/ebin \
    -pa _build/default/lib/cowlib/ebin \
    -pa _build/default/lib/ranch/ebin \
    -pa _build/default/lib/jsx/ebin \
    -pa _build/default/lib/jose/ebin \
    -eval "auction_app:start_node($HTTP_PORT)" \
    -noshell

