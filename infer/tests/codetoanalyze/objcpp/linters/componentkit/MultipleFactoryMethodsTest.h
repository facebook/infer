/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#import <Foundation/Foundation.h>

#import "FakeComponentKitHeader.h"

@interface FooComponent : CKCompositeComponent // OK - only one factory method
+ (instancetype)newWithObject:(NSObject*)obj;
@end

@interface BarComponent : CKCompositeComponent // Not OK - two factory methods
+ (instancetype)newWithObject1:(NSObject*)obj;
+ (instancetype)newWithObject2:(NSObject*)obj;
@end

@interface BazComponent : CKCompositeComponent // OK - no factory methods (?)
@end

// Not OK - Using the class name instaed of `instancename` doesn't make this OK
@interface BadComponent : CKCompositeComponent
+ (instancetype)newWithObject1:(NSObject*)obj;
+ (BadComponent*)newWithObject2:(NSObject*)obj;
@end

// OK - don't count non-factory methods
@interface OKComponent : CKCompositeComponent
+ (instancetype)newWithObject:(NSObject*)obj;
+ (FooComponent*)newOtherWithObject:(NSObject*)obj; // different class
+ (int)somethingElse;
- (NSString*)blah:(int)lol;
@end
