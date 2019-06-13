/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/Foundation.h>

@interface B : NSObject
@end

@implementation B
+ (void)newWithB {
}

+ (void)foo {
}

@end

@interface A<NSObject>
@end

@implementation A

- (int)foo {
  return 0;
}

+ (int)newWithBA {
  return 0;
}

@end
