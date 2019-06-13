/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
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
