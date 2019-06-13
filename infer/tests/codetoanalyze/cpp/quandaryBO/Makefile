# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.
TESTS_DIR = ../../..

OPTIONS_1 = \
  --quandaryBO-only --passthroughs --debug-exceptions --filtering \
  --project-root $(TESTS_DIR) \

OPTIONS_2 = \
   --quandaryBO-only --quandary --enable-issue-type UNTRUSTED_BUFFER_ACCESS \
   --passthroughs --debug-exceptions --filtering \
   --project-root $(TESTS_DIR) \

OPTIONS_3 = \
    --quandaryBO-only --bufferoverrun --enable-issue-type BUFFER_OVERRUN_U5 \
    --passthroughs --debug-exceptions --filtering \
    --project-root $(TESTS_DIR) \

OPTIONS_4 = \
    --quandaryBO-only --bufferoverrun --debug-exceptions --no-filtering \
    --project-root $(TESTS_DIR) \

test: test1 test2 test3 test4

test1:
	$(MAKE) -f multitest.make test TEST_SUFFIX=-t1 TEST_RESULT_SUFFIX=-t1 INFER_OPTIONS="$(OPTIONS_1)"

test2:
	$(MAKE) -f multitest.make test TEST_SUFFIX=-t2 TEST_RESULT_SUFFIX=-t2 INFER_OPTIONS="$(OPTIONS_2)"

test3:
	$(MAKE) -f multitest.make test TEST_SUFFIX=-t3 TEST_RESULT_SUFFIX=-t3 INFER_OPTIONS="$(OPTIONS_3)"

test4:
	$(MAKE) -f multitest.make test TEST_SUFFIX=-t4 TEST_RESULT_SUFFIX=-t4 INFER_OPTIONS="$(OPTIONS_4)"

replace: replace1 replace2 replace3 replace4

replace1:
	$(MAKE) -f multitest.make replace TEST_SUFFIX=-t1 TEST_RESULT_SUFFIX=-t1 INFER_OPTIONS="$(OPTIONS_1)"

replace2:
	$(MAKE) -f multitest.make replace TEST_SUFFIX=-t2 TEST_RESULT_SUFFIX=-t2 INFER_OPTIONS="$(OPTIONS_2)"

replace3:
	$(MAKE) -f multitest.make replace TEST_SUFFIX=-t3 TEST_RESULT_SUFFIX=-t3 INFER_OPTIONS="$(OPTIONS_3)"

replace4:
	$(MAKE) -f multitest.make replace TEST_SUFFIX=-t4 TEST_RESULT_SUFFIX=-t4 INFER_OPTIONS="$(OPTIONS_4)"

clean:
	$(MAKE) -f multitest.make clean TEST_SUFFIX=-t1
	$(MAKE) -f multitest.make clean TEST_SUFFIX=-t2
	$(MAKE) -f multitest.make clean TEST_SUFFIX=-t3
	$(MAKE) -f multitest.make clean TEST_SUFFIX=-t4
