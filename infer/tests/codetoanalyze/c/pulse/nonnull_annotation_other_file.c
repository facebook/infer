/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// nonnull_annotation.c declares this function with the nonnull attribute
void declared_nonnull_in_one_file(
    const char* destination_buffer_that_the_function_writes_the_result_to);

void call_declared_nonnull_in_one_file_ok(void) {
  declared_nonnull_in_one_file("x");
}
