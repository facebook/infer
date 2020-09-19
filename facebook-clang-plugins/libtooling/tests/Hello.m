/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "FoundationStub.h"

@interface Hello : NSObject
- (void)say:(int)i;
@end

@implementation Hello
- (void)say:(int)i {
  if (i > 0) {
    NSLog(@"Hello, world! (%d)", i);
  }
}
@end

int main(int argc, char *argv[]) {
  for (int i = 0; i < 3; i++) {
    @autoreleasepool {
      [[Hello new] say:i];
    }
  }
  return 0;
}
