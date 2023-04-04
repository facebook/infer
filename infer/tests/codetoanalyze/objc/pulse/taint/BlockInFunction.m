/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/Foundation.h>

typedef void (^BlockType)(NSString* source);

void call_block_log(BlockType completion) {}

void log_string(NSString* source) {}

void source_block_passed_to_function_bad() {
  call_block_log(^(NSString* source) {
    log_string(source);
  });
}
