/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
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

@implementation C
@end
