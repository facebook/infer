/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/NSObject.h>

#import <string>
#import <unordered_map>

template <typename... T>
class MyClassTemplated {};

typedef MyClassTemplated<> MyClass;

@interface ReferenceTest : NSObject
// bug
+ (instancetype)newWithConstAction:(const MyClass&)action;

// bug
+ (instancetype)newWithActionRef:(MyClass&)action;

// bug
+ (instancetype)newWithAction:(MyClass)action;

// no bug
+ (instancetype)newWithTypedAction:(MyClassTemplated<BOOL>)typedAction;

@end

@implementation ReferenceTest
// bug
+ (instancetype)newWithConstAction:(const MyClass&)action {
  return nil;
}
// bug
+ (instancetype)newWithActionRef:(MyClass&)action {
  return nil;
}
// bug
+ (instancetype)newWithAction:(MyClass&)action {
  return nil;
}

// This should not produce an error
+ (instancetype)newWithTypedAction:(MyClassTemplated<BOOL>)typedAction {
  return nil;
}

@end
