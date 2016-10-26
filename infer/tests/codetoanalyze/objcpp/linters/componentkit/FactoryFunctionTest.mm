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

// Fake "component" (i.e. shouldn't fire for functions that return these)
@interface BarComponent : NSObject
@end

@interface FooComponent : CKCompositeComponent
@end

static FooComponent* ExampleFunctionBeforeImpl() { return nil; }

static BarComponent* ExampleSkipFunctionBeforeImpl() { return nil; }

@implementation FooComponent

+ (instancetype)newWithObject:(NSObject*)obj {
}
static FooComponent* ExampleFunctionInsideImpl() { return nil; }

static BarComponent* ExampleSkipFunctionInsideImpl() { return nil; }

@end

static FooComponent* ExampleFunctionAfterImpl() { return nil; }

static BarComponent* ExampleSkipFunctionAfterImpl() { return nil; }
