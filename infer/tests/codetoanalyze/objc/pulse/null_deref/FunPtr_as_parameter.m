/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/NSObject.h>

typedef void (*MyFunPtr)(NSObject*);

@interface BFunPtr : NSObject {
  int y;
  int x;
}
- (void)call_funptr:(MyFunPtr)funptr;

@end

@implementation BFunPtr

- (void)call_funptr:(MyFunPtr)funptr {
  (*funptr)(self);
}

void assign_5_to_x(BFunPtr* self) { self->x = 5; }

- (int)f {
  [self call_funptr:(&assign_5_to_x)];
  return self->x + self->y;
}

void call_BFunPtr_f_npe_bad() {
  BFunPtr* b = [[BFunPtr alloc] init];
  b->y = 10;
  int z = [b f];
  if (z == 15) {
    int* p = NULL;
    *p = 42;
  }
}

void call_BFunPtr_f_no_npe_good() {
  BFunPtr* b = [[BFunPtr alloc] init];
  b->y = 10;
  int z = [b f];
  if (z == 1) {
    int* p = NULL;
    *p = 42;
  }
}
@end
