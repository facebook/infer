# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

CLANG_BUILD_FLAGS=-stdlib=libc++ -g -o1 -fsanitize=fuzzer,address,signed-integer-overflow
FUZZ_BUILD_FLAGS=${CLANG_BUILD_FLAGS} -DFUZZ
KLEE_RUN_FLAGS=--use-query-log=all:kquery --use-query-log=solver:kquery --optimize --search=dfs -use-independent-solver=0 -use-cex-cache=0 --write-no-tests -use-forked-solver=0 --rewrite-equalities=0

default: klee_treiber

########################################
## KLEE

klee_clean:
	rm -rf *.bc *.mbc klee-out-* klee-last

klee_build:
	clang -DKLEE -I$$HOME/klee/include/klee -emit-llvm -c -g -O0 -Xclang -disable-O0-optnone *.c
	llvm-link scheduler.bc ref_stack.bc treiber_stack.bc test_treiber_stack.bc -o klee_treiber_stack.mbc
	llvm-link scheduler.bc ref_queue.bc test_folly_ProducerConsumerQueue.bc -o klee_folly_ProducerConsumerQueue.mbc
	llvm-link scheduler.bc ref_queue.bc test_ms_queue.bc -o klee_ms_queue.mbc

klee_treiber: clean klee_build
	time klee ${KLEE_RUN_FLAGS} klee_treiber_stack.mbc

klee_folly_ProducerConsumerQueue: clean klee_build
	time klee ${KLEE_RUN_FLAGS} klee_folly_ProducerConsumerQueue.mbc

klee_ms_queue: clean klee_build
	time klee ${KLEE_RUN_FLAGS} klee_ms_queue.mbc

########################################
## libfuzz fuzzing

fuzz_clean:
	rm -rf crash-* fuzzer_* *.dSYM fuzz_treiber fuzz_folly_ProducerConsumerQueue fuzz_ms_queue

fuzz_build:
	clang $(FUZZ_BUILD_FLAGS) scheduler.c ref_stack.c treiber_stack.c test_treiber_stack.c -o fuzz_treiber
	clang $(FUZZ_BUILD_FLAGS) scheduler.c ref_queue.c test_folly_ProducerConsumerQueue.c -o fuzz_folly_ProducerConsumerQueue
	clang $(FUZZ_BUILD_FLAGS) scheduler.c ref_queue.c test_ms_queue.c -o fuzz_ms_queue

fuzz_treiber: fuzz_clean fuzz_build
	time ./fuzz_treiber

fuzz_folly_ProducerConsumerQueue: fuzz_clean fuzz_build
	time ./fuzz_folly_ProducerConsumerQueue

fuzz_ms_queue: fuzz_clean fuzz_build
	time ./fuzz_ms_queue

########################################
## Completely randomized fuzzer

rand_clean:
	rm -rf crash-* fuzzer_* rand_treiber rand_folly_ProducerConsumerQueue rand_ms_queue

rand_build:
	clang ${CLANG_BUILD_FLAGS} -DRAND_FUZZ scheduler.c ref_stack.c treiber_stack.c test_treiber_stack.c -o rand_treiber
	clang ${CLANG_BUILD_FLAGS} -DRAND_FUZZ scheduler.c ref_queue.c test_folly_ProducerConsumerQueue.c -o rand_folly_ProducerConsumerQueue
	clang ${CLANG_BUILD_FLAGS} -DRAND_FUZZ scheduler.c ref_queue.c test_ms_queue.c -o rand_ms_queue

rand_treiber: rand_clean rand_build
	time ./rand_treiber

rand_folly_ProducerConsumerQueue: rand_clean rand_build
	time ./rand_folly_ProducerConsumerQueue

rand_ms_queue: rand_clean rand_build
	time ./rand_ms_queue

########################################
## Common stuff

clean: klee_clean fuzz_clean rand_clean

