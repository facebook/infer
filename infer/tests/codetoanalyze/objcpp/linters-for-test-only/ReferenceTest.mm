/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
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
