/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/NSObject.h>

@class MyClass1;
@class MyClass2;
@class MyClass3;

@interface MyClass1 : NSObject

@property(nonatomic, strong) MyClass2* b;
@end

@implementation MyClass1
@end

@interface MyClass2 : NSObject

@property(nonatomic, strong) MyClass3* c;
@end

@implementation MyClass2
@end

@interface MyClass3 : NSObject

@property(nonatomic, strong) MyClass1* a;
@end

@implementation MyClass3
@end

int strongcycle_length3_bad() {
  MyClass1* a_obj = [MyClass1 new];
  MyClass2* b_obj = [MyClass2 new];
  MyClass3* c_obj = [MyClass3 new];
  a_obj.b = b_obj;
  b_obj.c = c_obj;
  c_obj.a = a_obj; // Retain cycle
  return 0;
}
