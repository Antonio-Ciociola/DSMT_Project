#!/usr/bin/env bash

# Script to start a 3-node distributed auction cluster

COOKIE="auction_secret"
EBIN_DIR="ebin"

echo "=============================================="
echo "  Starting 3-Node Distributed Auction Cluster"
echo "=============================================="
echo ""

# Check if ebin directory exists
if [ ! -d "$EBIN_DIR" ]; then
    echo "Error: $EBIN_DIR directory not found. Please run 'make' first."
    exit 1
fi

# Get hostname
HOSTNAME=$(hostname -s)

echo "Starting Node 1 (Port 8080)..."
gnome-terminal -- bash -c "
    erl -sname node1 \
        -setcookie $COOKIE \
        -pa $EBIN_DIR \
        -pa _build/default/lib/cowboy/ebin \
        -pa _build/default/lib/cowlib/ebin \
        -pa _build/default/lib/ranch/ebin \
        -pa _build/default/lib/jsx/ebin \
        -eval 'auction_app:start_node(8080).'; exec bash" 2>/dev/null || \
echo "Open terminal 1 manually: ./run_master.sh node1 8080" &

sleep 3

echo "Starting Node 2 (Port 8081) and joining cluster..."
gnome-terminal -- bash -c "
    erl -sname node2 \
        -setcookie $COOKIE \
        -pa $EBIN_DIR \
        -pa _build/default/lib/cowboy/ebin \
        -pa _build/default/lib/cowlib/ebin \
        -pa _build/default/lib/ranch/ebin \
        -pa _build/default/lib/jsx/ebin \
        -eval 'auction_app:start_node(8081), timer:sleep(2000), auction_app:join_cluster(node1@$HOSTNAME).'; exec bash" 2>/dev/null || \
echo "Open terminal 2 manually: ./run_worker.sh node1 node2 8081" &

sleep 3

echo "Starting Node 3 (Port 8082) and joining cluster..."
gnome-terminal -- bash -c "
    erl -sname node3 \
        -setcookie $COOKIE \
        -pa $EBIN_DIR \
        -pa _build/default/lib/cowboy/ebin \
        -pa _build/default/lib/cowlib/ebin \
        -pa _build/default/lib/ranch/ebin \
        -pa _build/default/lib/jsx/ebin \
        -eval 'auction_app:start_node(8082), timer:sleep(2000), auction_app:join_cluster(node1@$HOSTNAME).'; exec bash" 2>/dev/null || \
echo "Open terminal 3 manually: ./run_worker.sh node1 node3 8082" &

echo ""
echo "=============================================="
echo "  Cluster Started!"
echo "=============================================="
echo ""
echo "WebSocket endpoints:"
echo "  - Node 1: ws://localhost:8080/ws"
echo "  - Node 2: ws://localhost:8081/ws"
echo "  - Node 3: ws://localhost:8082/ws"
echo ""
echo "Health check endpoints:"
echo "  - Node 1: http://localhost:8080/health"
echo "  - Node 2: http://localhost:8081/health"
echo "  - Node 3: http://localhost:8082/health"
echo ""
echo "To check cluster status, run in any node:"
echo "  server:get_nodes()."
echo "  mnesia:system_info(running_db_nodes)."
echo ""
