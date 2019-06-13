/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/Foundation.h>

#include <vector>
#include <stdio.h>

#import "FakeComponentKitHeader.h"

@interface CKLabelComponent : CKCompositeComponent
+ (instancetype)newWithLabelAttributes:(LabelAttributes)labelAttributes
                        viewAttributes:(ViewAttributes)viewAttributes
                                  size:(CKSize)size;
@end
@implementation CKLabelComponent
+ (instancetype)newWithLabelAttributes:(LabelAttributes)labelAttributes
                        viewAttributes:(ViewAttributes)viewAttributes
                                  size:(CKSize)size {
  return nil;
}
@end

// Non-component class should be linted if in the same translation unit as a
// component or controllerimplementation
@interface SomeClass : NSObject
@end
@implementation SomeClass {
  NSString* _foo;
}

- (instancetype)init {
  if (self = [super init]) {
    NSString* foo = @"HI"; // error
    _foo = foo;
  }
  return nil;
}
@end

@interface FooComponent : CKCompositeComponent
@end
@implementation FooComponent
+ (instancetype)newWithString:(NSString*)string {
  // Built-in types
  int builtin1 = 3; // error
  const int builtin2 = 4; // no error
  int const builtin3 = 1; // no error

  // Objc types
  NSString* a = @"lol"; // error
  NSString* const b = @"lol"; // no error
  static NSString* st = @"lol"; // no error
  const NSString* c = @"lol"; // error
  const NSString* const d = @"lol"; // no error

  // Typedef resolution
  BOOL e = YES; // error
  const BOOL f = YES; // no error

  // Pointer types
  NSObject* const o1 = nil; // no error
  NSObject* const* o2 = &o1; // error
  NSObject* const* const o3 = &o1; // no error

  return [super newWithComponent:[CKLabelComponent newWithLabelAttributes:{
                  .string = [@[ a, b, c, d ] componentsJoinedByString:@", "],
                }
                                     viewAttributes:{}
                                     size:{}]];
}
@end

class BarClass {
 public:
  int a;
};

@interface BarComponent : CKCompositeComponent
@end
@implementation BarComponent
+ (instancetype)new {
  // C++ classes
  BarClass s1; // error
  const BarClass& s2 = s1; // no error
  BarClass& s3 = s1; // error
  const BarClass s4 = {.a = 3}; // no error

  // Whitelisted C++ class
  CKComponentScope w1(self); // no error
  const CKComponentScope& w2 = w1; // no error
  CKComponentScope& w3 = w1; // no error
  // const CKComponentScope w4 = {.a = 3}; // Can't, lacks default ctor
  const CKComponentScope w4(self); // no error

  // Whitelisted Objc class
  NSError* const e = nil; // no error
  NSError* __autoreleasing* e1; // no error
  NSError const* __autoreleasing* e2; // no error
  NSError* const* e3 = &e; // no error
  NSError* const* const e4 = &e; // no error

  // implicit vars (e.g. ones generated in a for loop statement)
  const std::vector<const int> const_vector_of_const_int = {1, 2, 3};
  for (int i : const_vector_of_const_int) { // error
    printf("%d", i);
  }
  for (const int i : const_vector_of_const_int) {
    printf("%d", i);
  }
  for (auto i : const_vector_of_const_int) { // auto != const int; error
    printf("%d", i);
  }
  for (const auto i : const_vector_of_const_int) {
    printf("%d", i);
  }
  for (auto& i : const_vector_of_const_int) { // auto == const int; no error
    printf("%d", i);
  }

  return nil;
}

- (void)no_mutable_local_variable_in_self_aliases {
  __weak auto weakSelf = self; // no error
  __strong auto strongSelf = self; // no error
  auto comp = [BarComponent new]; // error
}

@end
