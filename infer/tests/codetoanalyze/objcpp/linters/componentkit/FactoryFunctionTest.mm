/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
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
