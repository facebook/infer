/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// Test the functionality of the --linters-def-folder flag

#import <Foundation/NSObject.h>

@interface A : NSObject
@end

@implementation A
@end

@interface B : A
@end

@implementation B
@end

@interface ForbiddenClassName : NSObject
@end

@implementation ForbiddenClassName
@end

@interface C : NSObject
@end

@implementation C {
  __strong A* _app1;
  __weak A* _app2;
  A* _app3;
  __unsafe_unretained A* _app4;
}
@end
