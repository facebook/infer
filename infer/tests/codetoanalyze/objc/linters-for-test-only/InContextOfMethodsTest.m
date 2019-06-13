/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
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
