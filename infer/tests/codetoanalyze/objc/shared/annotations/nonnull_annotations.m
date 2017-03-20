/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
#import <Foundation/NSObject.h>

@interface A : NSObject

@property A* child;

@end

@implementation A {
  int x;
}

- (instancetype)init {
  return self;
}

- (int)test1:(A*)a {
  A* aa = [a child];
  return aa->x;
}

- (int)test2:(nonnull A*)a {
  A* aa = [a child];
  return aa->x;
}

- (int)test3:(void (^)(NSString*))successBlock {
  successBlock(@"Yay");
  return 0;
}

- (int)test4:(void (^_Nonnull)(NSString*))successBlock {
  successBlock(@"Yay");
  return 0;
}

@end
