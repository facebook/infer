/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/Foundation.h>

void addNilInDictBad(NSMutableDictionary* mDict) {
  id value = nil;
  [mDict setObject:value forKey:@"somestring"];
}

void addNSNullInDictOk(NSMutableDictionary* mDict) {
  [mDict setObject:[NSNull null] forKey:@"somestring"];
}

void addObjectInDictOk(NSMutableDictionary* mDict) {
  NSString* o = [NSString new];
  [mDict setObject:o forKey:@"somestring"];
}

void addObjectKeyNilInDictBad(NSMutableDictionary* mDict) {
  NSString* o = [NSString new];
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

void dictionaryRemoveObjectForKeyIfKeyExists(NSMutableDictionary* mDict,
                                             id key) {
  id value = [mDict objectForKey:key];
  if (value) {
    [mDict removeObjectForKey:key];
  }
}

void dictionaryRemoveObjectForKeyIfKeyExistsNilOK(NSMutableDictionary* mDict) {
  dictionaryRemoveObjectForKeyIfKeyExists(mDict, nil);
}

void dictionaryRemoveObjectForKeyIfKeyExistsNotNilOK(
    NSMutableDictionary* mDict) {
  dictionaryRemoveObjectForKeyIfKeyExists(mDict, @"somestring");
}

void dictionaryRemoveObjectForKeyIfSubscriptExists(NSMutableDictionary* mDict,
                                                   id key) {
  id value = mDict[key];
  if (value) {
    [mDict removeObjectForKey:key];
  }
}

void dictionaryRemoveObjectForKeyIfSubscriptExistsNilOK(
    NSMutableDictionary* mDict) {
  dictionaryRemoveObjectForKeyIfSubscriptExists(mDict, nil);
}

void dictionaryRemoveObjectForKeyIfSubscriptExistsNotNilOK(
    NSMutableDictionary* mDict) {
  dictionaryRemoveObjectForKeyIfSubscriptExists(mDict, @"somestring");
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
  [mArray addObject:[NSString new]];
}

void insertNilInArrayBad(NSMutableArray* mArray) {
  [mArray insertObject:nil atIndex:0];
}

void insertObjectInArrayOk(NSMutableArray* mArray) {
  [mArray insertObject:[NSString new] atIndex:0];
}

void replaceNilInArrayBad(NSMutableArray* mArray) {
  [mArray replaceObjectAtIndex:0 withObject:nil];
}

void replaceObjectInArrayOk(NSMutableArray* mArray) {
  [mArray replaceObjectAtIndex:0 withObject:[NSString new]];
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
      mArray, indexset, @[ [NSString new] ]);
}

void replaceObjectsAtNilIndexesWithObjectsInArrayBad(NSMutableArray* mArray) {
  replaceObjectsAtIndexesWithObjectsInArray(mArray, nil, @[ [NSString new] ]);
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

void dictionaryWithObjectsForKeysIfKeyExists(NSDictionary* dict, id key) {
  id value = [dict objectForKey:key];
  if (value) {
    dict = [NSDictionary dictionaryWithObjects:@[ value ] forKeys:@[ key ]];
  }
}

void dictionaryWithObjectsForKeysIfKeyExistsNilOK(NSDictionary* dict) {
  dictionaryWithObjectsForKeysIfKeyExists(dict, nil);
}

void dictionaryWithObjectsForKeysIfKeyExistsNotNilOK(NSDictionary* dict) {
  dictionaryWithObjectsForKeysIfKeyExists(dict, @"somestring");
}

void dictionaryWithObjectsForKeysIfSubcriptExists(NSDictionary* dict, id key) {
  id value = dict[key];
  if (value) {
    dict = [NSDictionary dictionaryWithObjects:@[ value ] forKeys:@[ key ]];
  }
}

void dictionaryWithObjectsForKeysIfSubscriptExistsNilOK(NSDictionary* dict) {
  dictionaryWithObjectsForKeysIfSubscriptExists(dict, nil);
}

void dictionaryWithObjectsForKeysIfSubscriptExistsNotNilOK(NSDictionary* dict) {
  dictionaryWithObjectsForKeysIfSubscriptExists(dict, @"somestring");
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

NSLocale* ret_bridge() {
  CFLocaleRef nameRef = CFLocaleCreate(NULL, NULL);
  return CFBridgingRelease(nameRef);
}

void createWithBrideInArrayOk() {
  NSLocale* locale = ret_bridge();
  NSArray<NSLocale*>* locales = @[ locale ];
}

id stringConstValueOK(NSMutableDictionary* dict, NSString* key) {
  id value; // unrelated 0 caused FP to manifest
  if (!key) {
    key = @"key"; // NSString.stringWithUTF8String is called to construct @"key"
  } // We need to model NSString.stringWithUTF8String to know that key is not
    // nil at this point
  NSMutableDictionary* subDict = [dict objectForKey:key];
  if (!subDict) {
    subDict = [NSMutableDictionary new];
    [dict setObject:subDict forKey:key];
  }
  return value;
}

NSString* get_object_from_array(NSArray<NSString*>* array) {
  if (array.count == 0) {
    return nil;
  }

  return [array objectAtIndex:0];
}

// missing models for collections
void pass_non_empty_array_ok_FP(NSMutableArray<NSString*>* mArray) {
  NSString* object = get_object_from_array(@[ @"element" ]);
  [mArray addObject:object];
}

void dictionaryInsertZeroNilBad(NSString* s) {
  NSDictionary* dict = @{
    @"key" : s ?: 0
  }; // 0 here is equivalent to nil (id(0)) as we expect an object to be
     // inserted into collection
}

void dictionaryInsertZeroLiteralOk(NSString* s) {
  NSDictionary* dict = @{@"key" : s ?: @0};
}
