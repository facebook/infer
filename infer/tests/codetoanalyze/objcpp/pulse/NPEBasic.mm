/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/Foundation.h>
#include <memory>

@interface AnotherObject : NSObject
- (int)someMethod:(int)param1;

- (NSObject*)unknown_function;
@end

@implementation AnotherObject
- (int)someMethod:(int)param1 {
  return param1;
}
@end

@interface SomeObject : NSObject

@property int x;
@property std::shared_ptr<int> ptr;
@property AnotherObject* anotherObject;

- (int)returnsPOD;

- (std::shared_ptr<int>)returnsnonPOD;

- (int)add:(int)param1 andParam2:(int)param2;

- (int*)getXPtr;

+ (SomeObject*)unknown;

+ (SomeObject*)returnsNil;

- (SomeObject*)get;

+ (NSString*)returnsStringNil;
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

- (int*)getXPtr {
  return &_x;
}

+ (SomeObject*)returnsNil {
  return nil;
}

- (int)callAnotherObjectMethod {
  return [_anotherObject someMethod:[self returnsPOD]];
}

- (SomeObject*)get {
  SomeObject* o = [SomeObject new];
  return o;
}

+ (NSString*)returnsStringNil {
  return nil;
}

+ (instancetype)sharedInstance {
  static SomeObject* shared;
  static dispatch_once_t onceToken;
  dispatch_once(&onceToken, ^{
    auto obj = [SomeObject unknown];
    if (obj) {
      shared = [SomeObject new];
    }
  });
  return shared;
}

- (int)methodNilDereferenceBad {
  int* x = nil;
  return *x;
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

std::shared_ptr<int> testCallMethodReturnsnonPODBad() {
  SomeObject* obj = nil;
  std::shared_ptr<int> d = [obj returnsnonPOD]; // UB
  return d;
}

std::shared_ptr<int> testSkippedOK() {
  SomeObject* obj = [[SomeObject unknown] get];
  std::shared_ptr<int> result = [obj returnsnonPOD];
  return result;
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

std::shared_ptr<int> testCallMethodReturnsnonPODLatentOk(bool b) {
  return testCallMethodReturnsnonPODLatent(false);
}

int testAccessPropertyAccessorOk() {
  SomeObject* obj = nil;
  return obj.x; // calls property accessor method
}

std::shared_ptr<int> testAccessPropertyAccessorBad() {
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

int testTraceBad() {
  SomeObject* obj = nil;
  int* ptr = [obj getXPtr];
  return *ptr;
}

void testUnknownNilSpecOk() {
  NSString* const str = [SomeObject returnsStringNil];
  if (str.length == 0) {
    return;
  };
  NSDictionary* dict = @{@"helloString" : str};
}

std::shared_ptr<int> unknown_call_twice_FP() {
  if (![SomeObject sharedInstance]) {
    return std::make_shared<int>(0);
  }
  return [[SomeObject sharedInstance] returnsnonPOD];
}

int testAnotherObjectUseSelfOk() {
  auto const obj = [SomeObject returnsNil];
  return [obj callAnotherObjectMethod];
}

void testCallNullptrBad() {
  void (*f)() = nullptr;
  f();
}

typedef void (^MutatorBlock)();

@interface Mutator : NSObject

- (NSObject*)setCallback:(MutatorBlock)block;

@end

@implementation Mutator {
  NSObject* _internalField;
}

- (NSObject*)setCallback:(MutatorBlock)block {
  return _internalField;
}

@end

void nilMessagingToFunctionWithBlockParamOk() {
  Mutator* const mutator = nil;
  NSObject* const initial = [mutator setCallback:^(){
  }];
}

void unrelated_invalidation(SomeObject* obj) {
  AnotherObject* ao = obj.anotherObject;
  int y = 0; /* unrelated invalidation */
  NSArray<NSObject*>* const constraints = @[ [ao unknown_function] ];
  return 0;
}

@interface Result
- (const std::shared_ptr<int>&)shared_ptr_of_int_ref;
@end

@interface Builder : NSObject
- (Result*)getResult;
@end

@implementation Builder : NSObject
- (Result*)getResult {
  return nil;
}
@end

const std::shared_ptr<int>& nilMessagingSharedPtrRefBad(Builder* builder) {
  return [[builder getResult] shared_ptr_of_int_ref];
}
