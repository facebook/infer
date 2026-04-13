BENCHDIR := $(dir $(abspath $(lastword $(MAKEFILE_LIST))))
PYDANTIC_TGZ := $(BENCHDIR)pydantic.tgz
PYDANTIC_DIR := $(BENCHDIR)pydantic
SCRIPT := $(BENCHDIR)run_benchs.py
CONFIG := $(BENCHDIR)config_12-31-25.py

.PHONY: bench clean

MAX_LINES := 1200
TIMEOUT := 60
DEBUG :=

DEBUG_FLAG := $(if $(filter true,$(DEBUG)),--debug,)

bench: $(PYDANTIC_DIR)
	python3 $(SCRIPT) $(PYDANTIC_DIR) --config $(CONFIG) --max-lines $(MAX_LINES) --timeout $(TIMEOUT) $(DEBUG_FLAG)

$(PYDANTIC_DIR): $(PYDANTIC_TGZ)
	tar xzf $(PYDANTIC_TGZ) -C $(BENCHDIR)
	touch $@

clean:
	rm -rf $(PYDANTIC_DIR)
