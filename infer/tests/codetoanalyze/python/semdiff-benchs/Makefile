BENCHDIR := $(dir $(abspath $(lastword $(MAKEFILE_LIST))))
PYDANTIC_TGZ := $(BENCHDIR)pydantic.tgz
PYDANTIC_DIR := $(BENCHDIR)pydantic
FASTAPI_TGZ := $(BENCHDIR)fastapi.tgz
FASTAPI_DIR := $(BENCHDIR)fastapi
SCRIPT := $(BENCHDIR)run_benchs.py
CONFIG := $(BENCHDIR)config_12-31-25.py

.PHONY: bench bench-pydantic bench-fastapi bench-large clean

MAX_LINES := 1200
TIMEOUT := 60
DEBUG :=

DEBUG_FLAG := $(if $(filter true,$(DEBUG)),--debug,)

bench: bench-pydantic

bench-pydantic: $(PYDANTIC_DIR)
	python3 $(SCRIPT) $(PYDANTIC_DIR) --config $(CONFIG) --max-lines $(MAX_LINES) --timeout $(TIMEOUT) $(DEBUG_FLAG)

bench-large: $(PYDANTIC_DIR) $(FASTAPI_DIR)
	python3 $(SCRIPT) $(PYDANTIC_DIR) $(FASTAPI_DIR) --config $(CONFIG) --min-lines 1000 --max-lines 10000 --timeout $(TIMEOUT) $(DEBUG_FLAG)

bench-fastapi: $(FASTAPI_DIR)
	python3 $(SCRIPT) $(FASTAPI_DIR) --config $(CONFIG) --min-lines 100 --max-lines 3000 --timeout $(TIMEOUT) $(DEBUG_FLAG)

$(PYDANTIC_DIR): $(PYDANTIC_TGZ)
	tar xzf $(PYDANTIC_TGZ) -C $(BENCHDIR)
	touch $@

$(FASTAPI_DIR): $(FASTAPI_TGZ)
	tar xzf $(FASTAPI_TGZ) -C $(BENCHDIR)
	touch $@

clean:
	rm -rf $(PYDANTIC_DIR) $(FASTAPI_DIR)
