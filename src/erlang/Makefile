# Makefile for Distributed Auction System

.PHONY: all clean distclean deps compile test

# Directories
EBIN_DIR = ebin
REBAR3 = $(shell which rebar3)

all: deps compile
	@echo ""
	@echo "Build complete!"
	@echo "Compiled files are in $(EBIN_DIR)/"
	@echo ""
	@echo "WebSocket endpoint: ws://localhost:8080/ws"
	@echo ""
	@echo "To start master node: ./run_master.sh"
	@echo "To start worker node:  ./run_worker.sh <master_node>"

deps:
	@echo "Fetching dependencies (cowboy, jsx)..."
	@$(REBAR3) get-deps

compile: $(EBIN_DIR)
	@echo "Compiling Erlang modules..."
	@$(REBAR3) compile
	@echo "Copying compiled beam files to $(EBIN_DIR)..."
	@cp _build/default/lib/auction_app/ebin/*.beam $(EBIN_DIR)/ 2>/dev/null || true
	@cp _build/default/lib/auction_app/ebin/*.app $(EBIN_DIR)/ 2>/dev/null || true

$(EBIN_DIR):
	@mkdir -p $(EBIN_DIR)

clean:
	@echo "Cleaning build artifacts..."
	@rm -rf $(EBIN_DIR) erl_crash.dump Mnesia.* mnesia_data
	@$(REBAR3) clean

distclean: clean
	@echo "Removing dependencies..."
	@rm -rf _build rebar.lock

test: all
	@echo "Running tests..."
	@$(REBAR3) eunit

.PHONY: all clean distclean test
