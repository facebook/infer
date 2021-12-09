/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/NSObject.h>

typedef void (^MyBlock)();

@interface B : NSObject {
  int y;
  int x;
}
- (void)call_block:(MyBlock)block;

@end

@implementation B

- (void)call_block:(MyBlock)block {
  block();
}

- (int)f {
  [self call_block:^{
    self->x = 5;
  }];
  return self->x + self->y;
}

void call_f_npe_bad() {
  B* b = [[B alloc] init];
  b->y = 10;
  int z = [b f];
  if (z == 15) {
    int* p = NULL;
    *p = 42;
  }
}

void call_f_no_npe_good() {
  B* b = [[B alloc] init];
  b->y = 10;
  int z = [b f];
  if (z == 1) {
    int* p = NULL;
    *p = 42;
  }
}
@end
