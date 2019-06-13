/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import "CADisplayLink.h"

@implementation CADisplayLink

// Returns a new display link.
// The model retains strongly the target object.
+ (CADisplayLink*)displayLinkWithTarget:(id)target selector:(SEL)sel {

  CADisplayLink* c = [CADisplayLink alloc];
  c->_target = target;
  c->_selector = sel;

  return c;
}

// Release the target
- (void)invalidate {
  self->_target = nil;
}

@end
