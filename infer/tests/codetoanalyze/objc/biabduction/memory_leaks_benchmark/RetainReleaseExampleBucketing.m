/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/NSObject.h>

@interface RRB : NSObject

@end

@implementation RRB

- init {
  return self;
}

@end

// no leak in bucketing cf mode
void RetainReleaseTest() { RRB* a = [[RRB alloc] init]; }
