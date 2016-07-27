/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#include <Foundation/NSObject.h>
#include <stdlib.h>

@interface A : NSObject

@property char* buffer;

@end

@implementation A

- (instancetype)initWithBuffer {
  _buffer = malloc(sizeof(char));
  return self;
}

- (void)dealloc {
  free(_buffer);
}

@end

int main() {
  [[A alloc] initWithBuffer];
  return 0;
}
