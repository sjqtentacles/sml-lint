# sml-lint build
#
#   make            build the test binary with MLton (default)
#   make test       build + run tests under MLton
#   make test-poly  run tests under Poly/ML
#   make all-tests  run the suite under both compilers
#   make example    build + run the demo (deterministic lint report)
#   make clean      remove build artifacts
#
# Layout B (vendoring): own sources in src/; the sml-mlast frontend is vendored
# byte-for-byte under lib/ and loaded first.

MLTON      ?= mlton
BIN        := bin
LIBROOT    := lib/github.com/sjqtentacles
TEST_MLB   := test/sources.mlb
SRCS       := $(wildcard $(LIBROOT)/sml-mlast/* src/* test/*.sml) $(TEST_MLB)

.PHONY: all test poly test-poly all-tests example clean

all: $(BIN)/test-mlton

$(BIN)/test-mlton: $(SRCS) | $(BIN)
	$(MLTON) -output $@ $(TEST_MLB)

test: $(BIN)/test-mlton
	$(BIN)/test-mlton

poly: $(BIN)/test-poly

$(BIN)/test-poly: $(SRCS) tools/polybuild | $(BIN)
	sh tools/polybuild -o $@ $(TEST_MLB)

test-poly: $(BIN)/test-poly
	$(BIN)/test-poly

all-tests: test test-poly

example: $(BIN)/demo
	./$(BIN)/demo

$(BIN)/demo: $(SRCS) examples/demo.sml examples/sources.mlb | $(BIN)
	$(MLTON) -output $@ examples/sources.mlb

$(BIN):
	mkdir -p $(BIN)

clean:
	rm -rf $(BIN)
