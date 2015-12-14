/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#import <Foundation/Foundation.h>
#import <QuartzCore/QuartzCore.h>


@interface A : NSObject
@property (nonatomic, strong) CADisplayLink *displayLink;

- (void) bla;
- (void) invalidate;
@end

@implementation A

- init {
    _displayLink = [CADisplayLink displayLinkWithTarget:self selector:@selector(bla)];

    return self;

}

- (void) bla {};

- (void) invalidate {
    // unregister displayLink target
    [_displayLink invalidate];

};

- (void) dealloc {
    [self dealloc];
}

@end


void testCycle() {

    A* a = [[A alloc] init];
    A* b = a;

}

void testNoCycle() {

    A* a = [[A alloc] init];
    [a invalidate]; //break the cycle
    [a dealloc];

}
