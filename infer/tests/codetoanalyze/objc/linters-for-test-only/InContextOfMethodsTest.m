/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
@interface InContextOfMethodsTest

@end

@implementation InContextOfMethodsTest

- (void)method {
  int x = 0;
}

- (void)method_with_block {
  ^{
    int x = 0;
  }();
}

@end

void function() { int x = 0; }
