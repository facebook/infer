# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

all: build-targets build-dune stats

build-targets:
	make -C java_targets

build-dune:
	dune build --root scan_jar

stats:
	python3 scripts/get_stats.py

clean:
	dune clean --root scan_jar
	rm -rf java_targets/jar
