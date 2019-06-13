/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/Foundation.h>
#import "Library.h"

int* __nullable returnsNull();

typedef struct s_ {
  int x;
} S;

@interface T : NSObject
- (NSObject* _Nullable)nullableMethod;
- (T* _Nullable)nullableT;
@property(nonatomic) S structProperty;
@property(nonatomic) S* pointerProperty;
@property(nonatomic, nullable) NSObject* nullableProperty;
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

- (NSArray*)nullablePropertyInNSArrayBad {
  return @[ self.nullableMethod ]; // reports here
}

- (NSArray*)nullableMethodCheckedForNullAndReturnOkay {
  NSObject* nullableObject = [self nullableMethod];
  NSArray* array;
  if (nullableObject == nil) {
    return array;
  }
  array = @[ nullableObject ]; // does not report here
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

- (NSDictionary*)nullableValueInNSDictionaryBad {
  NSObject* nullableValue = [self nullableMethod];
  NSMutableDictionary* dict = [NSMutableDictionary
      dictionaryWithObjectsAndKeys:@"key", nullableValue, nil]; // reports here
  return dict;
}

- (NSDictionary*)nullableKeyInNSDictionaryBad {
  NSObject* nullableKey = [self nullableMethod];
  NSMutableDictionary* dict = [NSMutableDictionary
      dictionaryWithObjectsAndKeys:nullableKey, @"value", nil]; // reports here
  return dict;
}

- (NSDictionary*)nullableKeyInNSDictionaryInitBad {
  NSObject* nullableKey = [self nullableMethod];
  NSDictionary* dict = [[NSDictionary alloc]
      initWithObjectsAndKeys:nullableKey, @"value", nil]; // reports here
  return dict;
}

- (NSDictionary*)nullableValueInNSDictionaryInitBad {
  NSObject* nullableValue = [self nullableMethod];
  NSDictionary* dict = [[NSDictionary alloc]
      initWithObjectsAndKeys:@"key", nullableValue, nil]; // reports here
  return dict;
}

- (NSDictionary*)nullableKeyInNSDictionaryInitLiteralBad {
  NSObject* nullableKey = [self nullableMethod];
  NSDictionary* dict = @{nullableKey : @"value"}; // reports here
  return dict;
}

- (NSDictionary*)nullableValueInNSDictionaryInitLiteralBad {
  NSObject* nullableValue = [self nullableMethod];
  NSDictionary* dict = @{@"key" : nullableValue}; // reports here
  return dict;
}

- (NSDictionary*)indirectNullableKeyInNSDictionaryBad {
  NSObject* nullableKey = [self nullableMethod];
  NSString* nullableKeyString = [nullableKey description];
  NSDictionary* dict = [[NSDictionary alloc]
      initWithObjectsAndKeys:nullableKeyString, @"value", nil]; // reports here
  return dict;
}

- (NSArray*)createArrayByAddingNilBad {
  NSArray* array = @[ [NSObject alloc] ];
  return [array arrayByAddingObject:[self nullableMethod]];
}

- (NSDictionary*)setNullableObjectInDictionaryBad {
  NSMutableDictionary* mutableDict = [NSMutableDictionary dictionary];
  [mutableDict setObject:[self nullableMethod] forKey:@"key"]; // reports here
  return mutableDict;
}

- (NSArray*)addNullableObjectInMutableArrayBad {
  NSMutableArray* mutableArray = [[NSMutableArray alloc] init];
  [mutableArray addObject:[self nullableMethod]]; // reports here
  return mutableArray;
}

- (NSArray*)insertNullableObjectInMutableArrayBad {
  NSMutableArray* mutableArray = [[NSMutableArray alloc] init];
  [mutableArray insertObject:[self nullableMethod] atIndex:0]; // reports here
  return mutableArray;
}

- (NSArray*)propagateNullabilityOnMethodCallBad {
  NSObject* nullableObject = [self nullableMethod];
  NSString* nullableString =
      [nullableObject description]; // returns nil if nullableObject is nil
  return @[ nullableString ]; // reports here
}

- (S*)shouldPropagateNullabilityOnPointerTypeBad {
  T* nullableT = [self nullableT];
  S* s = nullableT.pointerProperty; // returns  nil when nullableT is nil
  s->x = 42; // reports here
  return s;
}

- (S)shouldNotPropagateNullabilityOnNonPointerTypeGood {
  T* nullableT = [self nullableT];
  S s =
      nullableT.structProperty; // returns an empty struct when nullabeT is nil
  s.x = 42; // does not report here
  return s;
}

- (NSArray*)pointerAssignmentWithSubtypeOkay:(NSString*)string {
  NSObject* nullableObject = [self nullableMethod];
  nullableObject = string;
  NSArray* array = @[ nullableObject ]; // does not report here
  return array;
}

- (NSArray*)dereferenceLibraryMethodOk:(L*)object {
  NSObject* nullableObject = [object libraryMethod];
  NSArray* array = @[ nullableObject ]; // does not report here
  return array;
}

- (NSArray*)dereferenceNullableLibraryMethodBad:(L*)object {
  NSObject* nullableObject = [object nullableLibraryMethod];
  NSArray* array = @[ nullableObject ]; // reports here
  return array;
}

@end

@protocol P
- (NSObject* _Nullable)nullableMethod;
@end

NSDictionary* callNullableMethodFromProtocolBad(id<P> pObject) {
  NSObject* nullableObject = [pObject nullableMethod];
  return @{@"key" : nullableObject};
}
