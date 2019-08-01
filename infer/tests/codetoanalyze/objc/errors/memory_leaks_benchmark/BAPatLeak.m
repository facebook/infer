/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <Foundation/NSObject.h>
#include <stdlib.h>

typedef struct ABFDataRef {
} ABFDataRef;

// if impl is there --biabduction-fallback-model-alloc-pattern wont catch it
ABFDataRef* ABFDataCreate(size_t size);
void ABRelease(ABFDataRef*);

@interface A : NSObject
@end

@implementation A

- (void)someMethod_bad {
  ABFDataRef* someData = ABFDataCreate(4);
  // this allocates a new ABFData structure
}

- (void)someMethod_ok {
  ABFDataRef* someData = ABFDataCreate(4);
  // this allocates a new ABFData structure
  ABFRelease(someData);
}

@end
