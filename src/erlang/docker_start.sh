#!/usr/bin/env bash

# Script to start auction node in Docker (non-interactive)

NODE_NAME="${NODE_NAME:-auction}"
NODE_ROLE="${NODE_ROLE:-master}"
HTTP_PORT="${HTTP_PORT:-8081}"
MASTER_NODE="${MASTER_NODE:-}"
COOKIE="auction_secret"
EBIN_DIR="ebin"

echo "=============================================="
echo "  Starting Auction Node in Docker"
echo "  Node: ${NODE_NAME}"
echo "  Role: ${NODE_ROLE}"
echo "  HTTP Port: ${HTTP_PORT}"
if [ "$NODE_ROLE" = "slave" ] && [ -n "$MASTER_NODE" ]; then
    echo "  Master Node: ${MASTER_NODE}"
fi
echo "=============================================="
echo ""

# Determine startup command based on role
if [ "$NODE_ROLE" = "master" ]; then
    STARTUP_CMD="auction_app:start_master_node($HTTP_PORT)"
elif [ "$NODE_ROLE" = "slave" ] && [ -n "$MASTER_NODE" ]; then
    # Slave nodes: wait a bit for master to be ready, then connect
    STARTUP_CMD="timer:sleep(5000), auction_app:start_slave_node($HTTP_PORT, '$MASTER_NODE')"
else
    # Default to regular node start
    STARTUP_CMD="auction_app:start_node($HTTP_PORT)"
fi

# Start Erlang node without interactive shell, keeping it running
exec erl -sname "$NODE_NAME" \
    -setcookie "$COOKIE" \
    -pa "$EBIN_DIR" \
    -pa _build/default/lib/cowboy/ebin \
    -pa _build/default/lib/cowlib/ebin \
    -pa _build/default/lib/ranch/ebin \
    -pa _build/default/lib/jsx/ebin \
    -pa _build/default/lib/jose/ebin \
    -auction_app http_port "$HTTP_PORT" \
    -auction_app node_role "$NODE_ROLE" \
    -eval "$STARTUP_CMD" \
    -noshell

