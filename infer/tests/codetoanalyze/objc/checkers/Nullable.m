/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
#import <Foundation/Foundation.h>

int* __nullable returnsNull();

@interface T : NSObject
- (NSObject* _Nullable)nullableMethod;
@end

@implementation T {
  int* unnanotatedField;
  int* __nullable nullableField;
  int* nonnullField;
}

- (void)assignNullableFieldToNullOkay {
  nullableField = nil;
}

- (void)assignUnnanotatedFieldToNullBad {
  unnanotatedField = nil;
}

- (void)assignNonnullFieldToNullBad {
  nonnullField = nil;
}

- (void)testNullableFieldForNullOkay {
  if (nullableField == nil) {
  }
}

- (void)testUnnanotatedFieldForNullBad {
  if (unnanotatedField == nil) {
  }
}

- (int)DeadStoreFP_testUnnanotatedFieldInClosureBad {
  int (^testField)(int defaultValue);
  testField = ^(int defaultValue) {
    if (unnanotatedField != nil) {
      return *unnanotatedField;
    } else {
      return defaultValue;
    }
  };
  return testField(42);
}

- (void)testNonnullFieldForNullBad {
  if (nonnullField == nil) {
  }
}

- (void)dereferenceUnnanotatedFieldOkay {
  *unnanotatedField = 42;
}

- (void)dereferenceNonnullFieldOkay {
  *nonnullField = 42;
}

- (void)dereferenceNullableFieldBad {
  *nullableField = 42;
}

- (void)dereferenceUnnanotatedFieldAfterTestForNullBad {
  if (unnanotatedField == nil) {
    *unnanotatedField = 42;
  }
}

- (void)FP_dereferenceNonnullFieldAfterTestForNullOkay {
  if (nonnullField == nil) {
    *nonnullField = 42;
  }
}

- (void)dereferenceNullableFunctionBad {
  int* p = returnsNull();
  *p = 42;
}

- (void)dereferenceNullableFunction1Ok {
  int* p = returnsNull();
  if (p) {
    *p = 42;
  }
}

- (void)dereferenceNullableFunction2Ok {
  int* p = returnsNull();
  if (p != nil) {
    *p = 42;
  }
}

- (NSObject* _Nullable)nullableMethod {
  return nil;
}

- (NSString*)dereferenceNullableMethodOkay {
  NSObject* nullableObject = [self nullableMethod];
  return [nullableObject description]; // does not report here
}

- (void)reassigningNullableObjectOkay {
  NSObject* nullableObject = [self nullableMethod];
  nullableObject = nil; // does not report here
}

- (NSArray*)nullableObjectInNSArrayBad {
  NSObject* nullableObject = [self nullableMethod];
  NSArray* array = @[ nullableObject ]; // reports here
  return array;
}

- (NSArray*)secondElementNullableObjectInNSArrayBad {
  NSObject* allocatedObject = [NSObject alloc];
  NSObject* nullableObject = [self nullableMethod];
  NSArray* array = @[ allocatedObject, nullableObject ]; // reports here
  return array;
}

- (NSArray*)nullableObjectInNSArrayOkay {
  NSObject* nullableObject = [self nullableMethod];
  NSArray* array;
  if (nullableObject) {
    array = @[ nullableObject ]; // reports here
  } else {
    array = @[ @"String" ];
  }
  return array;
}

- (NSArray*)URLWithStringOkay {
  NSURL* url = [NSURL URLWithString:@"some/url/string"];
  NSArray* array = @[ url ]; // reports here
}

- (NSDictionary*)nullableValueInNSDictionary {
  NSObject* nullableValue = [self nullableMethod];
  NSMutableDictionary* dict = [NSMutableDictionary
      dictionaryWithObjectsAndKeys:@"key", nullableValue, nil]; // reports here
  return dict;
}

- (NSDictionary*)nullableKeyInNSDictionary {
  NSObject* nullableKey = [self nullableMethod];
  NSMutableDictionary* dict = [NSMutableDictionary
      dictionaryWithObjectsAndKeys:nullableKey, @"value", nil]; // reports here
  return dict;
}

@end
