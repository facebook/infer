/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/Foundation.h>
#include <memory>

@interface SomeObject : NSObject

@property int x;
@property int* x_ptr;
@property std::shared_ptr<int> ptr;

- (std::shared_ptr<int>)returnsnonPOD;

- (int*)getXPtr;

+ (SomeObject*)returnsNil;

- (SomeObject*)get;
@end

@implementation SomeObject

- (std::shared_ptr<int>)returnsnonPOD {
  return std::shared_ptr<int>(new int(_x));
}

- (int*)getXPtr {
  return &_x;
}

+ (SomeObject*)returnsNil {
  return nil;
}

- (SomeObject*)get {
  SomeObject* o = [SomeObject new];
  return o;
}
@end

int dereferenceNilBad() {
  int* int_ptr = nil;
  return *int_ptr;
}

std::shared_ptr<int> testCallMethodReturnsnonPODBad() {
  SomeObject* obj = nil;
  std::shared_ptr<int> d = [obj returnsnonPOD]; // UB
  return d;
}

std::shared_ptr<int> testNonPODTraceBad() {
  SomeObject* obj = [[SomeObject returnsNil] get];
  std::shared_ptr<int> result = [obj returnsnonPOD];
  return result;
}

std::shared_ptr<int> testCallMethodReturnsnonPODLatent(bool b) {
  SomeObject* obj;
  if (b) {
    obj = nil;
  } else {
    obj = [SomeObject new];
  }
  std::shared_ptr<int> d = [obj returnsnonPOD]; // UB if obj nil
  return d;
}

std::shared_ptr<int> testCallMethodReturnsnonPODLatentBad(bool b) {
  return testCallMethodReturnsnonPODLatent(true);
}

std::shared_ptr<int> testAccessPropertyAccessorBad() {
  SomeObject* obj = nil;
  return obj.ptr; // calls property accessor method, but return type is non-POD
}

int testTraceBad() {
  SomeObject* obj = nil;
  int* ptr = [obj getXPtr];
  return *ptr;
}

void testCallNullptrBad() {
  void (*f)() = nullptr;
  f();
}

NSString* returns_nil() { return nil; }

void nilInsertionIntoCollectionBad(NSMutableArray<NSString*>* mArray) {
  NSString* object = returns_nil();
  [mArray addObject:object];
}

void nilInsertionIntoCollectionThroughCallBad() {
  NSDictionary* dictionary = @{@"key" : returns_nil()};
}

void property_accessorBad() {
  SomeObject* obj = nil;
  *(obj.x_ptr) = 42;
}
