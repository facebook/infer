/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <Foundation/NSObject.h>
#include <stdlib.h>

@interface BufferContainer1 : NSObject

@property char* buffer;

@end

@implementation BufferContainer1

- (instancetype)init {
  _buffer = malloc(sizeof(char));
  return self;
}

- (void)dealloc {
  free(_buffer);
}

@end

@interface BufferContainer2 : NSObject

@property char* buffer;

@end

@implementation BufferContainer2

- (instancetype)init {
  _buffer = malloc(sizeof(char));
  return self;
}

- (void)dealloc {
}

@end

@interface Araii : NSObject

@property BufferContainer1* container;

@end

@implementation Araii

- (instancetype)init {
  _container = [[BufferContainer1 alloc] init];
  return self;
}

@end

/* b goes out of scope, this would cause b->_container to be leaked,
however, dealloc is called and _container is freed there, so no leak. */
void memory_leak_raii_no_leak_ok() {
  BufferContainer1* b = [[BufferContainer1 alloc] init];
}

/* b goes out of scope, this causes b->_container to be leaked. Even though
dealloc is called, _container is not freed there. */
void memory_leak_raii_leak_bad() {
  BufferContainer2* b = [[BufferContainer2 alloc] init];
}

/* a goes out of scope, this causes a->b->_container to be leaked. This is a FP
because dealloc is called, and it should call dealloc of b which would free
_container. This behaviour is still to be implemented. */
void memory_leak_raii_no_leak_ok_FP() { Araii* a = [[Araii alloc] init]; }
