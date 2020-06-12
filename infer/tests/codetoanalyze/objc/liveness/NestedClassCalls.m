/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/NSObject.h>

@interface NestedClass : NSObject
@property int val;
@end

bool myrand();

@implementation NestedClass
- (instancetype)myDoO:(id)o {
  return self;
}
+ (instancetype)initWithVal:(int)v {
  NestedClass* a = [NestedClass alloc];
  a.val = v;
  return a;
}
+ (instancetype)nestedGood {
  const BOOL isB = myrand();
  return [[[NestedClass initWithVal:isB ? 0 : 1] myDoO:[NSObject class]]
      myDoO:[NSObject class]];
}

+ (instancetype)nestedBad {
  const BOOL isB = myrand();
  return [[[NestedClass initWithVal:42] myDoO:[NSObject class]]
      myDoO:[NSObject class]];
}

@end
