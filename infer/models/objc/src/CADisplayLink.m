/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
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
