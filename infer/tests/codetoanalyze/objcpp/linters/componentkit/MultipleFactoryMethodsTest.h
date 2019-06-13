/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
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

@property int x;

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

// OK - unavailable initializers shouldn't count
@interface UnavailableInitializer1 : CKCompositeComponent
+ (instancetype)new // shouldn't count since it's unavailable
    __attribute__((unavailable("Must use designated initializer")));
+ (instancetype)newWithObject:(NSObject*)obj;
@end

// Not OK - there are 2 static initialzers, even w/out the unavailable one
@interface UnavailableInitializer2 : CKCompositeComponent
+ (instancetype)new // shouldn't count since it's unavailable
    __attribute__((unavailable("Must use designated initializer")));
+ (instancetype)newWithObject1:(NSObject*)obj;
+ (instancetype)newWithObject2:(NSObject*)obj;
@end

// 4 initializers -> should output 3 issues
@interface LotsOfInitializers : CKCompositeComponent
+ (instancetype)newWithObject1:(NSObject*)obj;
+ (instancetype)newWithObject2:(NSObject*)obj;
+ (instancetype)newWithObject3:(NSObject*)obj;
+ (instancetype)newWithObject4:(NSObject*)obj;
@end
