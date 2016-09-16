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

@interface Araii : NSObject

@property char* buffer;

@end

@implementation Araii

- (instancetype)initWithBuffer {
  _buffer = malloc(sizeof(char));
  return self;
}

- (void)dealloc {
  free(_buffer);
}

@end

int memory_leak_raii_main() {
  [[Araii alloc] initWithBuffer];
  return 0;
}
