/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <memory>
#include <string>
#include <vector>

#import <Foundation/NSObject.h>

struct Simple {
  int f;
};

@interface PulseTest : NSObject

- (int)deref_deleted_in_objc_method_bad;

@end

@implementation PulseTest

- (int)deref_deleted_in_objc_method_bad {
  auto* s = new Simple{1};
  delete s;
  Simple tmp = *s;
}

@end

void deref_deleted_bad() {
  auto* s = new Simple{1};
  delete s;
  Simple tmp = *s;
}
