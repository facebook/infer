/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/Foundation.h>

NSSet* tainted_set(void);
void* create_tainted(void);
id create_tainted_id(void);
void* create_untainted(void);
id create_untainted_id(void);
void testNSSet_sink(void* ptr) {
  if (ptr) {
    *ptr;
  }
}

void testNSSet_SetWithArrayBad(void) {
  NSArray* arg = (__bridge NSArray* _Nonnull)(create_tainted());
  NSSet* propagated = [NSSet setWithArray:arg];
  testNSSet_sink((__bridge void*)(propagated));
}

void testNSSet_SetWithObjectBad(void) {
  id arg = create_tainted_id();
  NSSet* propagated = [NSSet setWithObject:arg];
  testNSSet_sink((__bridge void*)(propagated));
}

void testNSSet_SetWithObjectsBad(void) {
  id arg = create_tainted_id();
  NSSet* propagated = [NSSet setWithObjects:arg];
  testNSSet_sink((__bridge void*)(propagated));
}

void testNSSet_SetWithObjectsCountBad(void) {
  id count = create_tainted_id();
  id const* arg = &count;
  NSSet* propagated = [NSSet setWithObjects:arg count:count];
  testNSSet_sink((__bridge void*)(propagated));
}

void testNSSet_SetByAddingObjectBad(void) {
  NSSet* set = [NSSet new];
  id arg = create_tainted_id();
  NSSet* propagated = [set setByAddingObject:arg];
  testNSSet_sink((__bridge void*)(propagated));
}

void testNSSet_SetByAddingObjectGood(void) {
  NSSet* set = tainted_set();
  id arg = @"something";
  [set setByAddingObject:arg];
  testNSSet_sink(&arg);
}

void testNSSet_SetByAddingObjectsFromSetBad(void) {
  NSSet* set = [NSSet new];
  NSSet* arg = tainted_set();
  NSSet* propagated = [set setByAddingObjectsFromSet:arg];
  testNSSet_sink((__bridge void*)(propagated));
}

void testNSSet_SetByAddingObjectsFromSetGood(void) {
  NSSet* set = tainted_set();
  NSSet* arg = [NSSet new];
  [set setByAddingObjectsFromSet:arg];
  testNSSet_sink((__bridge void*)(arg));
}

void testNSSet_SetByAddingObjectsFromArrayBad(void) {
  NSSet* set = [NSSet new];
  NSArray* arg = (__bridge NSArray* _Nonnull)(create_tainted());
  NSSet* propagated = [set setByAddingObjectsFromSet:arg];
  testNSSet_sink((__bridge void*)(propagated));
}

void testNSSet_SetByAddingObjectsFromArrayGood(void) {
  NSSet* set = tainted_set();
  NSArray* arg = [NSArray new];
  [set setByAddingObjectsFromSet:arg];
  testNSSet_sink((__bridge void*)(arg));
}

void testNSSet_InitWithArrayBad(void) {
  NSSet* set = [NSSet new];
  NSArray* arg = (__bridge NSArray* _Nonnull)(create_tainted());
  NSSet* propagated = [set initWithArray:arg];
  testNSSet_sink((__bridge void*)(propagated));
}

void testNSSet_InitWithArrayGood(void) {
  NSSet* set = tainted_set();
  NSArray* arg = [NSArray new];
  [set initWithArray:arg];
  testNSSet_sink((__bridge void*)(arg));
}

void testNSSet_InitWithObjectsBad(void) {
  NSSet* set = [NSSet new];
  id arg = create_tainted_id();
  NSSet* propagated = [set initWithObjects:arg];
  testNSSet_sink((__bridge void*)(propagated));
}

void testNSSet_InitWithObjectsGood(void) {
  NSSet* set = tainted_set();
  id arg = create_untainted_id();
  [set initWithObjects:arg];
  testNSSet_sink(&arg);
}

void testNSSet_InitWithObjectsCountBad(void) {
  NSSet* set = [NSSet new];
  id count = create_tainted_id();
  id const* arg = &count;
  NSSet* propagated = [set initWithObjects:arg count:count];
  testNSSet_sink((__bridge void*)(propagated));
}

void testNSSet_InitWithObjectsCountGood(void) {
  NSSet* set = tainted_set();
  id count = create_untainted_id();
  [set initWithObjects:&count count:count];
  testNSSet_sink(&count);
}

void testNSSet_InitWithSetBad(void) {
  NSSet* set = [NSSet new];
  NSSet* arg = tainted_set();
  NSSet* propagated = [set initWithSet:arg];
  testNSSet_sink((__bridge void*)(propagated));
}

void testNSSet_InitWithSetGood(void) {
  NSSet* set = tainted_set();
  NSSet* arg = [NSSet new];
  [set initWithSet:arg];
  testNSSet_sink((__bridge void*)(arg));
}

void testNSSet_InitWithSetCopyItemsBad(void) {
  NSSet* set = [NSSet new];
  NSSet* arg = tainted_set();
  NSSet* propagated = [set initWithSet:arg copyItems:false];
  testNSSet_sink((__bridge void*)(propagated));
}

void testNSSet_InitWithSetCopyItemsGood(void) {
  NSSet* set = tainted_set();
  NSSet* arg = [NSSet new];
  [set initWithSet:arg copyItems:false];
  testNSSet_sink((__bridge void*)(arg));
}

void testNSSet_CountBad(void) {
  NSSet* set = tainted_set();
  NSUInteger propagated = [set count];
  testNSSet_sink(&propagated);
}

void testNSSet_AllObjectsBad(void) {
  NSSet* set = tainted_set();
  NSArray* propagated = [set allObjects];
  testNSSet_sink((__bridge void*)(propagated));
}

void testNSSet_AnyObjectBad(void) {
  NSSet* set = tainted_set();
  id propagated = [set anyObject];
  testNSSet_sink(&propagated);
}

void testNSSet_ContainsObjectBad(void) {
  NSSet* set = [NSSet new];
  id arg = create_tainted_id();
  BOOL propagated = [set containsObject:arg];
  testNSSet_sink(&propagated);
}

void testNSSet_ContainsObjectGood(void) {
  NSSet* set = tainted_set();
  id arg = create_untainted_id();
  [set containsObject:arg];
  testNSSet_sink(&arg);
}

void testNSSet_FilteredSetUsingPredicateBad(void) {
  NSSet* set = [NSSet new];
  void* arg = create_tainted();
  NSSet* propagated =
      [set filteredSetUsingPredicate:(__bridge NSPredicate* _Nonnull)(arg)];
  testNSSet_sink((__bridge void*)(propagated));
}

void testNSSet_FilteredSetUsingPredicateGood(void) {
  NSSet* set = tainted_set();
  void* arg = create_untainted();
  [set filteredSetUsingPredicate:(__bridge NSPredicate* _Nonnull)(arg)];
  testNSSet_sink(arg);
}

void testNSSet_MakeObjectsPerformSelectorBad(void) {
  NSSet* set = [NSSet new];
  void* arg = create_tainted();
  [set makeObjectsPerformSelector:arg];
  testNSSet_sink((__bridge void*)(set));
}

void testNSSet_MakeObjectsPerformSelectorGood(void) {
  NSSet* set = tainted_set();
  void* arg = create_untainted();
  [set makeObjectsPerformSelector:arg];
  testNSSet_sink(arg);
}

void testNSSet_MakeObjectsPerformSelectorWithObjectBad(void) {
  NSSet* set = [NSSet new];
  void* arg = create_tainted();
  id obj = create_untainted_id();
  [set makeObjectsPerformSelector:arg withObject:obj];
  testNSSet_sink(&obj);
  testNSSet_sink((__bridge void*)(set));
}

void testNSSet_MakeObjectsPerformSelectorWithObjectGood(void) {
  NSSet* set = tainted_set();
  void* arg = create_untainted();
  id obj = create_tainted_id();
  [set makeObjectsPerformSelector:arg withObject:obj];
  testNSSet_sink(arg);
}

void testNSSet_MemberBad(void) {
  NSSet* set = [NSSet new];
  id arg = create_tainted_id();
  id propagated = [set member:arg];
  testNSSet_sink(&propagated);
}

void testNSSet_MemberGood(void) {
  NSSet* set = tainted_set();
  id arg = create_untainted_id();
  [set member:arg];
  testNSSet_sink(&arg);
}

void testNSSet_ObjectEnumeratorBad(void) {
  NSSet* set = tainted_set();
  NSEnumerator* propagated = [set objectEnumerator];
  testNSSet_sink((__bridge void*)(propagated));
}

void testNSSet_EnumerateObjectsUsingBlockBad(void) {
  NSSet* set = [NSSet new];
  void* arg = create_tainted();
  [set enumerateObjectsUsingBlock:(__bridge void (^_Nonnull)(
                                      id _Nonnull __strong, BOOL* _Nonnull))(
                                      arg)];
  testNSSet_sink((__bridge void*)(set));
}

void testNSSet_EnumerateObjectsUsingBlockGood(void) {
  NSSet* set = tainted_set();
  void* arg = create_untainted();
  [set enumerateObjectsUsingBlock:(__bridge void (^_Nonnull)(
                                      id _Nonnull __strong, BOOL* _Nonnull))(
                                      arg)];
  testNSSet_sink(arg);
}

void testNSSet_EnumerateObjectsWithOptionsUsingBlockBad(void) {
  NSSet* set = [NSSet new];
  void* arg = create_tainted();
  [set enumerateObjectsWithOptions:arg
                        usingBlock:(__bridge void (^_Nonnull)(
                                       id _Nonnull __strong, BOOL* _Nonnull))(
                                       arg)];
  testNSSet_sink((__bridge void*)(set));
}

void testNSSet_EnumerateObjectsWithOptionsUsingBlockGood(void) {
  NSSet* set = tainted_set();
  void* arg = create_untainted();
  [set enumerateObjectsWithOptions:arg
                        usingBlock:(__bridge void (^_Nonnull)(
                                       id _Nonnull __strong, BOOL* _Nonnull))(
                                       arg)];
  testNSSet_sink(arg);
}

void testNSSet_ObjectsPassingTestBad(void) {
  NSSet* set = [NSSet new];
  void* arg = create_tainted();
  NSSet* propagated =
      [set objectsPassingTest:(__bridge BOOL(^_Nonnull)(id _Nonnull __strong,
                                                        BOOL* _Nonnull))(arg)];
  testNSSet_sink((__bridge void*)(propagated));
}

void testNSSet_ObjectsPassingTestGood(void) {
  NSSet* set = tainted_set();
  void* arg = create_untainted();
  [set objectsPassingTest:(__bridge BOOL(^_Nonnull)(id _Nonnull __strong,
                                                    BOOL* _Nonnull))(arg)];
  testNSSet_sink(arg);
}

void testNSSet_ObjectsWithOptionsPassingTestBad(void) {
  NSSet* set = [NSSet new];
  void* arg = create_tainted();
  NSSet* propagated =
      [set objectsWithOptions:arg
                  passingTest:(__bridge BOOL(^_Nonnull)(id _Nonnull __strong,
                                                        BOOL* _Nonnull))(arg)];
  testNSSet_sink((__bridge void*)(propagated));
}

void testNSSet_ObjectsWithOptionsPassingTestGood(void) {
  NSSet* set = tainted_set();
  void* arg = create_untainted();
  [set objectsWithOptions:arg
              passingTest:(__bridge BOOL(^_Nonnull)(id _Nonnull __strong,
                                                    BOOL* _Nonnull))(arg)];
  testNSSet_sink(arg);
}

void testNSSet_IsSubsetOfSetBad(void) {
  NSSet* set = [NSSet new];
  NSSet* arg = tainted_set();
  BOOL propagated = [set isSubsetOfSet:arg];
  testNSSet_sink(&propagated);
}

void testNSSet_IsSubsetOfSetGood(void) {
  NSSet* set = [NSSet new];
  NSSet* arg = tainted_set();
  BOOL propagated = [set isSubsetOfSet:arg];
  testNSSet_sink((__bridge void*)(set));
}

void testNSSet_IntersectsSetBad(void) {
  NSSet* set = [NSSet new];
  NSSet* arg = tainted_set();
  BOOL propagated = [set intersectsSet:arg];
  testNSSet_sink(&propagated);
}

void testNSSet_IntersectsSetGood(void) {
  NSSet* set = [NSSet new];
  NSSet* arg = tainted_set();
  [set intersectsSet:arg];
  testNSSet_sink((__bridge void*)(set));
}

void testNSSet_IsEqualToSetBad(void) {
  NSSet* set = [NSSet new];
  NSSet* arg = tainted_set();
  BOOL propagated = [set isEqualToSet:arg];
  testNSSet_sink(&propagated);
}

void testNSSet_IsEqualToSetGood(void) {
  NSSet* set = [NSSet new];
  NSSet* arg = tainted_set();
  [set isEqualToSet:arg];
  testNSSet_sink((__bridge void*)(set));
}

void testNSSet_ValueForKeyBad(void) {
  NSSet* set = [NSSet new];
  void* arg = create_tainted();
  id propagated = [set valueForKey:(__bridge NSString* _Nonnull)(arg)];
  testNSSet_sink(&propagated);
}

void testNSSet_ValueForKeyGood(void) {
  NSSet* set = tainted_set();
  void* arg = create_untainted();
  [set valueForKey:(__bridge NSString* _Nonnull)(arg)];
  testNSSet_sink(arg);
}

void testNSSet_SetValueForKeyBad(void) {
  NSSet* set = [NSSet new];
  id arg = create_tainted_id();
  void* arg2 = create_tainted();
  [set setValue:arg forKey:(__bridge NSString* _Nonnull)(arg2)];
  testNSSet_sink((__bridge void*)(set));
}

void testNSSet_SetValueForKeyGood(void) {
  NSSet* set = tainted_set();
  id arg = create_untainted_id();
  void* arg2 = create_untainted();
  [set setValue:arg forKey:(__bridge NSString* _Nonnull)(arg2)];
  testNSSet_sink(&arg);
  testNSSet_sink(&arg2);
}

void testNSSet_SortedArrayUsingDescriptorsBad(void) {
  NSSet* set = [NSSet new];
  NSArray* arg = (__bridge NSArray* _Nonnull)(create_tainted());
  NSSet* propagated = [set sortedArrayUsingDescriptors:arg];
  testNSSet_sink((__bridge void*)(propagated));
}

void testNSSet_SortedArrayUsingDescriptorsGood(void) {
  NSSet* set = tainted_set();
  NSArray* arg = [NSArray new];
  [set sortedArrayUsingDescriptors:arg];
  testNSSet_sink((__bridge void*)(arg));
}

void testNSSet_DescriptionBad(void) {
  NSSet* set = tainted_set();
  NSString* propagated = [set description];
  testNSSet_sink((__bridge void*)(propagated));
}

void testNSSet_DescriptionWithLocaleBad(void) {
  NSSet* set = [NSSet new];
  id arg = create_tainted_id();
  NSString* propagated = [set descriptionWithLocale:arg];
  testNSSet_sink((__bridge void*)(propagated));
}

void testNSSet_DescriptionWithLocaleGood(void) {
  NSSet* set = tainted_set();
  id arg = create_untainted_id();
  [set descriptionWithLocale:arg];
  testNSSet_sink((__bridge void*)(arg));
}

void testNSSet_InitWithCoderBad(void) {
  NSSet* set = [NSSet new];
  void* arg = create_tainted();
  NSSet* propagated = [set initWithCoder:(__bridge NSCoder* _Nonnull)(arg)];
  testNSSet_sink((__bridge void*)(propagated));
}

void testNSSet_InitWithCoderGood(void) {
  NSSet* set = tainted_set();
  void* arg = create_untainted();
  [set initWithCoder:(__bridge NSCoder* _Nonnull)(arg)];
  testNSSet_sink(arg);
}

/* I fail to compile these testNSSet_s with
 * `error: no known class method for selector 'setWithCollectionViewIndexPath:'`

void testNSSet_SetWithCollectionViewIndexPathBad(void) {
void* arg = create_tainted();
NSSet* propagated = [NSSet setWithCollectionViewIndexPath:arg ];
testNSSet_sink((__bridge void*)(propagated));}

void testNSSet_SetWithCollectionViewIndexPathsBad(void) {
NSArray* arg = (__bridge NSArray* _Nonnull)(create_tainted());
NSSet* propagated = [NSSet setWithCollectionViewIndexPaths:arg ];
testNSSet_sink((__bridge void*)(propagated));}

void testNSSet_EnumerateIndexPathsWithOptionsUsingBlockBad(void) {
NSSet* set = [NSSet new];
void* arg = create_tainted();
NSSet* propagated = [set enumerateIndexPathsWithOptions:arg usingBlock:arg ];
testNSSet_sink((__bridge void*)(propagated));}

void testNSSet_EnumerateIndexPathsWithOptionsUsingBlockGood(void) {
NSSet* set = tainted_set();
void* arg = create_untainted();
NSSet* propagated = [set enumerateIndexPathsWithOptions:arg usingBlock:arg ];
testNSSet_sink(arg);}
*/
