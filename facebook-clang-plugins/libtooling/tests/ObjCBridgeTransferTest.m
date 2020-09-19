/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "FoundationStub.h"

typedef struct ABFDataRef {
} ABFDataRef;

@interface ABFData
@end

ABFDataRef* ABFDataCreate();

@interface A : NSObject
@end

@implementation A

- (void)bridge_transfer_example {
  ABFData* someData = (__bridge_transfer ABFData*)ABFDataCreate();
}

@end
