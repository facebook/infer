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

- (int)foo:(int)foo_par;

@end

@implementation A

- (int)foo:(int)foo_par {

  return foo_par;
}

@end

@interface B : A // Error: A subclass

- (void)bar;

@end

@implementation B

- (void)bar {
  A* a = [[A alloc] init];
  [a foo:5]; // Error: report MACRO_TEST1, MACRO_TEST2, MACRO_TEST3
}

@end

@interface C : B // Error: C subclass of B subclass of A
@end

@implementation C
@end

@interface D : C // Error: D subclass of C ... subclass of A
@end

@implementation D
@end

@interface E : D // Error: E subclass of D ... subclass of A
@end

@implementation E
@end

@interface F : NSObject
@end

@implementation F
@end
