/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <Foundation/NSObject.h>
#include <stdlib.h>

@interface MyBufferContainer : NSObject

@property char* buffer;

@end

@implementation MyBufferContainer

- (instancetype)init {
  if (self = [super init]) {
    _buffer = malloc(sizeof(char));
  }
  return self;
}

- (void)dealloc {
  free(_buffer);
}

@end
void raii_no_leak_ok() {
  MyBufferContainer* b = [[MyBufferContainer alloc] init];
}
