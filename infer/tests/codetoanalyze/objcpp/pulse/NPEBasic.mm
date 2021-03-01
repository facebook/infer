/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/NSObject.h>
#include <memory>

@interface SomeObject : NSObject

@property int x;
@property std::shared_ptr<int> ptr;

- (int)returnsPOD;

- (std::shared_ptr<int>)returnsnonPOD;

- (int)add:(int)param1 andParam2:(int)param2;

@end

@implementation SomeObject

- (int)returnsPOD {
  return _x;
}

- (std::shared_ptr<int>)returnsnonPOD {
  return std::shared_ptr<int>(new int(_x));
}

- (int)add:(int)param1 andParam2:(int)param2 {
  return _x + param1 + param2;
}

@end

int dereferenceNilBad() {
  int* int_ptr = nil;
  return *int_ptr;
}

int testCallMethodReturnsPODOk() {
  SomeObject* obj = nil;
  return [obj returnsPOD];
}

std::shared_ptr<int> FN_testCallMethodReturnsnonPODBad() {
  SomeObject* obj = nil;
  std::shared_ptr<int> d = [obj returnsnonPOD]; // UB
  return d;
}

int testAccessPropertyAccessorOk() {
  SomeObject* obj = nil;
  return obj.x; // calls property accessor method
}

std::shared_ptr<int> FN_testAccessPropertyAccessorBad() {
  SomeObject* obj = nil;
  return obj.ptr; // calls property accessor method, but return type is non-POD
}

int methodReturnsPOD(SomeObject* obj) { return [obj returnsPOD]; };

int methodReturnsPODNilOk() { return methodReturnsPOD(nil); };

int methodReturnsPODNotNilOK() {
  SomeObject* o = [SomeObject new];
  return methodReturnsPOD(o);
}

int testFalsyReturnedValueOk() {
  int x = testCallMethodReturnsPODOk();

  if (x != 0) {
    int* int_ptr = nil;
    return *int_ptr;
  }
}

int testParamsRemainTheSameOk() {
  SomeObject* obj = nil;
  int x1 = 0;
  int x2 = 5;
  int x = [obj add:x1 andParam2:x2];

  if (x1 != 0) {
    int* int_ptr = nil;
    return *int_ptr;
  }

  if (x2 != 5) {
    int* int_ptr = nil;
    return *int_ptr;
  }
}
