/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/NSObject.h>

@interface A : NSObject
@property int val;
@end

bool myrand();

@implementation A
- (instancetype)myDoO:(id)o {
  return self;
}
+ (instancetype)initWithVal:(int)v {
  A* a = [A alloc];
  a.val = v;
  return a;
}
+ (instancetype)nestedGood {
  const BOOL isB = myrand();
  return [[[A initWithVal:isB ? 0 : 1] myDoO:[NSObject class]]
      myDoO:[NSObject class]];
}

+ (instancetype)nestedBad {
  const BOOL isB = myrand();
  return [[[A initWithVal:42] myDoO:[NSObject class]] myDoO:[NSObject class]];
}

@end
