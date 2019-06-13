/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <unordered_map>

#import <Foundation/NSObject.h>

#pragma GCC diagnostic ignored "-Wundeclared-selector"

struct SomeStruct {
  NSString* someLabel;
};

@interface SomeButton : NSObject

+ (instancetype)newWithStruct:(SomeStruct)aStruct
                          map:(const std::unordered_map<int, NSString*>&)aMap
                       object:(id)anObject
                       number:(int)n;

@end

SomeButton* buttonComponent(void);
SomeButton* buttonComponent(void) {
  // flagging passing empty struct and map
  return [SomeButton newWithStruct:{} map:{} object:nil number:0];
};

SomeButton* anotherButtonComponent(void);
SomeButton* anotherButtonComponent(void) {
  return [SomeButton newWithStruct:{.someLabel = @"hi"}
                               map:{
                                 { 1, @"some title" }
                               }
                            object:@"a string object"
                            number:5];
};

struct CKComponentAction {
  CKComponentAction(SEL selector);
  CKComponentAction(id target, SEL selector);
};

@interface FBSomeComponent : NSObject
+ (instancetype)newWithAction:(CKComponentAction)action;
@end

void foo(NSObject* someObject) {
  [FBSomeComponent newWithAction:@selector(thisBad:)];

  [FBSomeComponent newWithAction:{ @selector(thisIsAlsoBad:) }];

  [FBSomeComponent newWithAction:{ someObject, @selector(thisIsGood:) }];
};
