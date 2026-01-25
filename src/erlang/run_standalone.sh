#!/usr/bin/env bash

# Script to start a standalone auction node (for testing)

NODE_NAME="${1:-standalone}"
HTTP_PORT="${2:-8080}"
COOKIE="auction_secret"
EBIN_DIR="ebin"

echo "=============================================="
echo "  Starting Standalone Auction Node"
echo "  Node: ${NODE_NAME}"
echo "  HTTP Port: ${HTTP_PORT}"
echo "=============================================="
echo ""

# Check if ebin directory exists
if [ ! -d "$EBIN_DIR" ]; then
    echo "Error: $EBIN_DIR directory not found. Please run 'make' first."
    exit 1
fi

# Start Erlang node with -sname (short name) instead of -name
erl -sname "$NODE_NAME" \
    -setcookie "$COOKIE" \
    -pa "$EBIN_DIR" \
    -pa _build/default/lib/cowboy/ebin \
    -pa _build/default/lib/cowlib/ebin \
    -pa _build/default/lib/ranch/ebin \
    -pa _build/default/lib/jsx/ebin \
    -pa _build/default/lib/jose/ebin \
    -eval "auction_app:start_node($HTTP_PORT)."
