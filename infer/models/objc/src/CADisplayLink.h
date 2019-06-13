/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/Foundation.h>

@interface CADisplayLink : NSObject {

  id _target;
  SEL _selector;
}

@property(readonly, nonatomic) CFTimeInterval duration;

@property(nonatomic) NSInteger frameInterval;

@property(getter=isPaused, nonatomic) BOOL paused;

@property(readonly, nonatomic) CFTimeInterval timestamp;

// Returns a new display link.
+ (CADisplayLink*)displayLinkWithTarget:(id)target selector:(SEL)sel;

// Release the target
- (void)invalidate;

@end
