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

ABFDataRef* ABFDataCreate(size_t size);
void ABFRelease(ABFDataRef*);

@interface A : NSObject
@end

@implementation A

- (void)create_no_release_leak_bad {
  ABFDataRef* someData = ABFDataCreate(4);
}

- (void)create_then_release_ok {
  ABFDataRef* someData = ABFDataCreate(4);
  ABFRelease(someData);
}

@end
