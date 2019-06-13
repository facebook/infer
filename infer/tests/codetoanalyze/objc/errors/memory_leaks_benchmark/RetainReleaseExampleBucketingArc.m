/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/NSObject.h>

@interface RRBA : NSObject

@end

@implementation RRBA

- init {
  return self;
}

@end

// no leak in bucketing cf mode
void RetainReleaseArcTest() { RRBA* a = [[RRBA alloc] init]; }
