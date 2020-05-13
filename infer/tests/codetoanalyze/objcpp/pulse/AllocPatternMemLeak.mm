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

@interface ABFData
@end

ABFDataRef* ABFDataCreate(size_t size);
void ABFRelease(ABFDataRef*);

ABFData* ABFBridgingRelease(ABFDataRef*);

namespace abf {
ABFData* BridgingRelease(ABFDataRef*);
}

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

- (void)bridge_no_leak_good {
  ABFData* someData = (__bridge_transfer ABFData*)ABFDataCreate(4);
}

- (void)custom_bridge_no_leak_good {
  ABFData* someData = ABFBridgingRelease(ABFDataCreate(4));
}

- (void)custom_bridge_namesapce_no_leak_good {
  ABFData* someData = abf::BridgingRelease(ABFDataCreate(4));
}

@end
