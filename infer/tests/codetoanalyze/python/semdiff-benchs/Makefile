BENCHDIR := $(dir $(abspath $(lastword $(MAKEFILE_LIST))))
PYDANTIC_TGZ := $(BENCHDIR)pydantic.tgz
PYDANTIC_DIR := $(BENCHDIR)pydantic
FASTAPI_TGZ := $(BENCHDIR)fastapi.tgz
FASTAPI_DIR := $(BENCHDIR)fastapi
MYPY_TGZ := $(BENCHDIR)mypy.tgz
MYPY_DIR := $(BENCHDIR)mypy
RICH_TGZ := $(BENCHDIR)rich.tgz
RICH_DIR := $(BENCHDIR)rich
SCRIPT := $(BENCHDIR)run_benchs.py
GEN_BATCH := $(BENCHDIR)gen_batch_json.py
BATCH_BENCH := $(BENCHDIR)run_batch_bench.py
BATCH_LARGE := $(BENCHDIR)batch_large.json
BATCH_LARGE_MUT := $(BENCHDIR)batch_large_mut.json
CONFIG := $(BENCHDIR)config_12-31-25.py

ALL_DIRS := $(PYDANTIC_DIR) $(FASTAPI_DIR) $(MYPY_DIR) $(RICH_DIR)

.PHONY: bench bench-pydantic bench-fastapi bench-large capture-large clean

MAX_LINES := 1200
TIMEOUT := 60
DEBUG :=

DEBUG_FLAG := $(if $(filter true,$(DEBUG)),--debug,)

bench: bench-pydantic

bench-pydantic: $(PYDANTIC_DIR)
	python3 $(SCRIPT) $(PYDANTIC_DIR) --config $(CONFIG) --max-lines $(MAX_LINES) --timeout $(TIMEOUT) $(DEBUG_FLAG)

bench-large: $(BATCH_LARGE) $(BATCH_LARGE_MUT)
	python3 $(BATCH_BENCH) $(BATCH_LARGE) --batch-mut $(BATCH_LARGE_MUT) --config $(CONFIG) --timeout $(TIMEOUT) $(DEBUG_FLAG)

bench-fastapi: $(FASTAPI_DIR)
	python3 $(SCRIPT) $(FASTAPI_DIR) --config $(CONFIG) --min-lines 100 --max-lines 3000 --timeout $(TIMEOUT) $(DEBUG_FLAG)

$(PYDANTIC_DIR): $(PYDANTIC_TGZ)
	tar xzf $(PYDANTIC_TGZ) -C $(BENCHDIR)
	touch $@

$(FASTAPI_DIR): $(FASTAPI_TGZ)
	tar xzf $(FASTAPI_TGZ) -C $(BENCHDIR)
	touch $@

$(MYPY_DIR): $(MYPY_TGZ)
	tar xzf $(MYPY_TGZ) -C $(BENCHDIR)
	touch $@

$(RICH_DIR): $(RICH_TGZ)
	tar xzf $(RICH_TGZ) -C $(BENCHDIR)
	touch $@

$(BATCH_LARGE) $(BATCH_LARGE_MUT) &: $(ALL_DIRS) $(GEN_BATCH)
	python3 $(GEN_BATCH) $(ALL_DIRS) --min-lines 1000 --max-lines 10000 -o $(BATCH_LARGE) --output-mutated $(BATCH_LARGE_MUT)

capture-large: $(ALL_DIRS)
	python3 $(GEN_BATCH) $(ALL_DIRS) --min-lines 1000 --max-lines 10000 -o $(BATCH_LARGE) --output-mutated $(BATCH_LARGE_MUT)

clean:
	rm -rf $(ALL_DIRS) $(BATCH_LARGE) $(BATCH_LARGE_MUT)
