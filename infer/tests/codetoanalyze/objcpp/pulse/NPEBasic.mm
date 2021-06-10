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
@property std::shared_ptr<int> ptr;

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

void addNilInDictBad(NSMutableDictionary* mDict) {
  id value = nil;
  [mDict setObject:value forKey:@"somestring"];
}

void addNSNullInDictOk(NSMutableDictionary* mDict) {
  [mDict setObject:[NSNull null] forKey:@"somestring"];
}

void addObjectInDictOk(NSMutableDictionary* mDict) {
  SomeObject* o = [SomeObject new];
  [mDict setObject:o forKey:@"somestring"];
}

void addObjectKeyNilInDictBad(NSMutableDictionary* mDict) {
  SomeObject* o = [SomeObject new];
  [mDict setObject:o forKey:nil];
}

void addObjectInDict(NSMutableDictionary* mDict, id value) {
  [mDict setObject:value forKey:@"somestring"];
}

void addNilInDictBracketsOk(NSMutableDictionary* mDict) {
  mDict[@"key2"] = nil; // Passing nil will cause any object corresponding
                        // to a key to be removed from the dictionary.
}

void addNilKeyInDictBracketsBad(NSMutableDictionary* mDict) {
  id key = nil;
  mDict[key] = @"somestring";
}

void addInDictBracketsOk(NSMutableDictionary* mDict) {
  mDict[@"key"] = @"somestring";
}

void removeObjectFromDict(NSMutableDictionary* mDict, id key) {
  [mDict removeObjectForKey:key];
}

void removeObjectFromDictKeyNilBad(NSMutableDictionary* mDict) {
  removeObjectFromDict(mDict, nil);
}

void removeObjectFromDictKeyNotNilOK(NSMutableDictionary* mDict) {
  removeObjectFromDict(mDict, @"somestring");
}

void dictionaryWithSharedKeySetOk() {
  id sharedKeySet = [NSDictionary sharedKeySetForKeys:@[ @"key1", @"key2" ]];
  NSMutableDictionary* mDict =
      [NSMutableDictionary dictionaryWithSharedKeySet:sharedKeySet];
}

void dictionaryWithSharedKeySetBad() {
  NSMutableDictionary* mDict =
      [NSMutableDictionary dictionaryWithSharedKeySet:nil];
}

void testNilMessagingForModelNilNilOK_FP() { addObjectInDict(nil, nil); }

void testNilMessagingForModelNilStringOK() {
  addObjectInDict(nil, @"somestring");
}

void testNilMessagingForModelNotNilDictBad(NSMutableDictionary* mDict) {
  addObjectInDict(mDict, nil);
}

void addNilInArrayBad(NSMutableArray* mArray) { [mArray addObject:nil]; }

void addObjectInArrayOk(NSMutableArray* mArray) {
  [mArray addObject:[SomeObject new]];
}

void insertNilInArrayBad(NSMutableArray* mArray) {
  [mArray insertObject:nil atIndex:0];
}

void insertObjectInArrayOk(NSMutableArray* mArray) {
  [mArray insertObject:[SomeObject new] atIndex:0];
}

void replaceNilInArrayBad(NSMutableArray* mArray) {
  [mArray replaceObjectAtIndex:0 withObject:nil];
}

void replaceObjectInArrayOk(NSMutableArray* mArray) {
  [mArray replaceObjectAtIndex:0 withObject:[SomeObject new]];
}

void removeObjectsAtIndexesFromArray(NSMutableArray* mArray, id indexset) {
  [mArray removeObjectsAtIndexes:indexset];
}

void removeObjectsAtIndexesFromArrayOK(NSMutableArray* mArray) {
  NSIndexSet* indexset = [NSIndexSet indexSetWithIndex:1];
  removeObjectsAtIndexesFromArray(mArray, indexset);
}

void removeObjectsAtIndexesFromArrayBad(NSMutableArray* mArray) {
  removeObjectsAtIndexesFromArray(mArray, nil);
}

void replaceObjectsAtIndexesWithObjectsInArray(NSMutableArray* mArray,
                                               id indexset,
                                               id objects) {
  [mArray replaceObjectsAtIndexes:indexset withObjects:objects];
}

void replaceObjectsAtIndexesWithObjectsInArrayOk(NSMutableArray* mArray) {
  NSIndexSet* indexset = [NSIndexSet indexSetWithIndex:0];
  replaceObjectsAtIndexesWithObjectsInArray(
      mArray, indexset, @[ [SomeObject new] ]);
}

void replaceObjectsAtNilIndexesWithObjectsInArrayBad(NSMutableArray* mArray) {
  replaceObjectsAtIndexesWithObjectsInArray(mArray, nil, @[ [SomeObject new] ]);
}

void replaceObjectsAtIndexesWithNilObjectsInArrayBad(NSMutableArray* mArray) {
  NSIndexSet* indexset = [NSIndexSet indexSetWithIndex:0];
  replaceObjectsAtIndexesWithObjectsInArray(mArray, indexset, nil);
}

void addInDictBracketsDefault(NSMutableDictionary<NSString*, NSString*>* mDict,
                              NSString* key) {
  mDict[key] = @"default";
}

void accessZeroElementOk(NSMutableDictionary<NSString*, NSString*>* mDict) {
  NSArray<NSString*>* array =
      [[NSUserDefaults standardUserDefaults] arrayForKey:@"key"];
  NSString* key = array[0];
  addInDictBracketsDefault(mDict, key);
}

void addObjectInMSet(NSMutableSet* mSet, id object) { [mSet addObject:object]; }

void addObjectInMSetOk(NSMutableSet* mSet) {
  addObjectInMSet(mSet, @"somestring");
}

void addObjectInMSetBad(NSMutableSet* mSet) { addObjectInMSet(mSet, nil); }

void removeObjectFromMSet(NSMutableSet* mSet, id object) {
  [mSet removeObject:object];
}

void removeObjectFromMSetOk(NSMutableSet* mSet) {
  removeObjectFromMSet(mSet, @"somestring");
}

void removeObjectFromMSetBad(NSMutableSet* mSet) {
  removeObjectFromMSet(mSet, nil);
}

void dictionaryLiteral(id key, id object) {
  NSDictionary* dict = @{key : object};
}

void dictionaryLiteralOk() { dictionaryLiteral(@"key", @"obj"); }

void dictionaryLiteralKeyNilBad() { dictionaryLiteral(nil, @"obj"); }

void dictionaryLiteralObjectNilBad() { dictionaryLiteral(@"key", nil); }

void dictionaryWithObjectsForKeysCount(id key, id object) {
  NSString* values[1];
  values[0] = object;

  NSString* keys[1];
  keys[0] = key;

  NSDictionary* dict = [NSDictionary dictionaryWithObjects:values
                                                   forKeys:keys
                                                     count:1];
}

void dictionaryWithObjectsForKeysCountOk() {
  dictionaryWithObjectsForKeysCount(@"key", @"somestring");
}

void FN_dictionaryWithObjectsForKeysCountKeyNilBad() {
  dictionaryWithObjectsForKeysCount(nil, @"somestring");
}

void FN_dictionaryWithObjectsForKeysCountObjectNilBad() {
  dictionaryWithObjectsForKeysCount(@"key", nil);
}

void dictionaryWithObjectForKey(id key, id object) {
  NSDictionary* dict = [NSDictionary dictionaryWithObject:object forKey:key];
}

void dictionaryWithObjectForKeyOk() {
  dictionaryWithObjectForKey(@"key", @"somestring");
}

void dictionaryWithObjectForKeyNilBad() {
  dictionaryWithObjectForKey(nil, @"somestring");
}

void dictionaryWithNilObjectForKeyBad() {
  dictionaryWithObjectForKey(@"key", nil);
}

void dictionaryWithSharedKeySetForKeys(id keys) {
  id sharedKeySet = [NSDictionary sharedKeySetForKeys:keys];
}

void dictionaryWithSharedKeySetForKeysOk() {
  dictionaryWithSharedKeySetForKeys(@[ @"key1", @"key2" ]);
}

void dictionaryWithSharedKeySetForKeysBad() {
  dictionaryWithSharedKeySetForKeys(nil);
}

void setWithObject(id object) { NSSet* set = [NSSet setWithObject:object]; }

void setWithObjectOk() { setWithObject(@"obj"); }

void setWithObjectBad() { setWithObject(nil); }

void setByAddingObject(id object) {
  NSSet* set = [NSSet set];
  set = [set setByAddingObject:object];
}

void setByAddingObjectOk() { setByAddingObject(@"obj"); }

void setByAddingObjectBad() { setByAddingObject(nil); }

void setWithObjectsForCount(id object) {
  NSString* values[1];
  values[0] = object;

  NSSet* set = [NSSet setWithObjects:values count:1];
}

void setWithObjectsForCountOk() { setWithObjectsForCount(@"obj"); }

void FN_setWithObjectsForCountBad() { setWithObjectsForCount(nil); }

void setInitWithObjectsForCount(id object) {
  NSString* values[1];
  values[0] = object;

  NSSet* set = [[NSSet alloc] initWithObjects:values count:1];
}

void setInitWithObjectsForCountOk() { setInitWithObjectsForCount(@"obj"); }

void FN_setInitWithObjectsForCountBad() { setInitWithObjectsForCount(nil); }

void arrayLiteral(id object) { NSArray* array = @[ object ]; }

void arrayLiteralOk() { arrayLiteral(@"obj"); }

void arrayLiteralNilBad() { arrayLiteral(nil); }

void arrayWithObject(id object) {
  NSArray* array = [NSArray arrayWithObject:object];
}

void arrayWithObjectOK() { arrayWithObject(@"obj"); }

void arrayWithObjectNilBad() { arrayWithObject(nil); }

void arrayWithObjects(id object) {
  NSString* values[1];
  values[0] = object;

  NSArray* array = [NSArray arrayWithObjects:values count:1];
}

void arrayWithObjectsOk() { arrayWithObjects(@"obj"); }

void FN_arrayWithObjectsNilBad() { arrayWithObjects(nil); }

std::shared_ptr<int> unknown_call_twice_FP() {
  if (![SomeObject sharedInstance]) {
    return std::make_shared<int>(0);
  }
  return [[SomeObject sharedInstance] returnsnonPOD];
}
