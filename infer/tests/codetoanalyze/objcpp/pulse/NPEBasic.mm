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

@end

@implementation SomeObject

- (int)returnsPOD {
  return _x;
}

- (std::shared_ptr<int>)returnsnonPOD {
  return std::shared_ptr<int>(new int(_x));
}

@end

int dereferenceNilBad() {
  int* int_ptr = nil;
  return *int_ptr;
}

int FP_testCallMethodReturnsPODOk() {
  SomeObject* obj = nil;
  return [obj returnsPOD];
}

std::shared_ptr<int> testCallMethodReturnsnonPODBad() {
  SomeObject* obj = nil;
  std::shared_ptr<int> d = [obj returnsnonPOD]; // UB
  return d;
}

int FP_testAccessPropertyAccessorOk() {
  SomeObject* obj = nil;
  return obj.x; // calls property accessor method
}

std::shared_ptr<int> testAccessPropertyAccessorBad() {
  SomeObject* obj = nil;
  return obj.ptr; // calls property accessor method, but return type is non-POD
}

int methodReturnsPOD(SomeObject* obj) { return [obj returnsPOD]; };

int FP_methodReturnsPODNilOk() { return methodReturnsPOD(nil); };

int methodReturnsPODNotNilOK() {
  SomeObject* o = [SomeObject new];
  return methodReturnsPOD(o);
}
