/*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
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
