/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/Foundation.h>

NSArray* tainted_array(void);
void* create_tainted(void);
id create_tainted_id(void);
void* create_untainted(void);
id create_untainted_id(void);
void testNSArray_sink(void* ptr) {
  if (ptr) {
    *ptr;
  }
}

void testNSArray_ArrayWithArrayBad(void) {
  void* arg = create_tainted();
  NSArray* propagated =
      [NSArray arrayWithArray:(__bridge NSArray* _Nonnull)(arg)];
  testNSArray_sink((__bridge void*)(propagated));
}

void testNSArray_ArrayWithObjectBad(void) {
  id arg = create_tainted_id();
  NSArray* propagated = [NSArray arrayWithObject:arg];
  testNSArray_sink((__bridge void*)(propagated));
}

void testNSArray_ArrayWithObjectsBad(void) {
  id arg = create_tainted_id();
  NSArray* propagated = [NSArray arrayWithObjects:arg];
  testNSArray_sink((__bridge void*)(propagated));
}

void testNSArray_ArrayWithObjectsCountBad(void) {
  NSArray* array = [NSArray new];
  id arg = create_tainted_id();
  __unsafe_unretained id arg2 = arg;
  NSArray* propagated = [NSArray arrayWithObjects:&arg2 count:arg];
  testNSArray_sink((__bridge void*)(propagated));
}

void testNSArray_InitWithArrayBad(void) {
  NSArray* array = [NSArray new];
  NSArray* arg = tainted_array();
  NSArray* propagated = [array initWithArray:arg];
  testNSArray_sink((__bridge void*)(propagated));
}

void testNSArray_InitWithArrayGood(void) {
  NSArray* array = tainted_array();
  NSArray* arg = [NSArray new];
  [array initWithArray:arg];
  testNSArray_sink((__bridge void*)(arg));
}

void testNSArray_InitWithArrayCopyItemsBad(void) {
  NSArray* array = [NSArray new];
  void* arg = create_tainted();
  NSArray* propagated =
      [array initWithArray:(__bridge NSArray* _Nonnull)(arg) copyItems:false];
  testNSArray_sink((__bridge void*)(propagated));
}

void testNSArray_InitWithArrayCopyItemsGood(void) {
  NSArray* array = tainted_array();
  void* arg = create_untainted();
  [array initWithArray:(__bridge NSArray* _Nonnull)(arg) copyItems:false];
  testNSArray_sink(arg);
}

void testNSArray_InitWithObjectsBad(void) {
  NSArray* array = [NSArray new];
  id arg = create_tainted_id();
  NSArray* propagated = [array initWithObjects:arg];
  testNSArray_sink((__bridge void*)(propagated));
}

void testNSArray_InitWithObjectsGood(void) {
  NSArray* array = tainted_array();
  id arg = create_untainted_id();
  [array initWithObjects:arg];
  testNSArray_sink(&arg);
}

void testNSArray_InitWithObjectsCountBad(void) {
  NSArray* array = [NSArray new];
  id arg = create_tainted_id();
  __unsafe_unretained id arg2 = arg;
  NSArray* propagated = [array initWithObjects:&arg2 count:arg];
  testNSArray_sink((__bridge void*)(propagated));
}

void testNSArray_InitWithObjectsCountGood(void) {
  NSArray* array = tainted_array();
  id arg = create_untainted_id();
  __unsafe_unretained id arg2 = arg;
  [array initWithObjects:&arg2 count:arg];
  testNSArray_sink(&arg);
}

void testNSArray_ContainsObjectBad(void) {
  NSArray* array = [NSArray new];
  id arg = create_tainted_id();
  BOOL propagated = [array containsObject:arg];
  testNSArray_sink(&propagated);
}

void testNSArray_ContainsObjectGood(void) {
  NSArray* array = tainted_array();
  id arg = create_untainted_id();
  [array containsObject:arg];
  testNSArray_sink(&arg);
}

void testNSArray_CountBad(void) {
  NSArray* array = tainted_array();
  NSUInteger propagated = [array count];
  testNSArray_sink(&propagated);
}

void testNSArray_GetObjectsRangeBad(void) {
  NSArray* array = tainted_array();
  id arg = create_untainted_id();
  __unsafe_unretained id arg2 = arg;
  [array getObjects:&arg2 range:*((NSRange*)&arg)];
  testNSArray_sink(&arg2);
}

void testNSArray_GetObjectsRangeGood(void) {
  NSArray* array = [NSArray new];
  id arg = create_tainted_id();
  __unsafe_unretained id arg2 = arg;
  [array getObjects:&arg2 range:*((NSRange*)&arg)];
  testNSArray_sink((__bridge void*)(array));
}

void testNSArray_FirstObjectBad(void) {
  NSArray* array = tainted_array();
  id propagated = [array firstObject];
  testNSArray_sink(&propagated);
}

void testNSArray_LastObjectBad(void) {
  NSArray* array = tainted_array();
  id propagated = [array lastObject];
  testNSArray_sink(&propagated);
}

void testNSArray_ObjectAtIndexBad(void) {
  NSArray* array = [NSArray new];
  id arg = create_tainted_id();
  NSArray* propagated = [array objectAtIndex:arg];
  testNSArray_sink((__bridge void*)(propagated));
}

void testNSArray_ObjectAtIndexGood(void) {
  NSArray* array = tainted_array();
  id arg = create_untainted_id();
  [array objectAtIndex:arg];
  testNSArray_sink(&arg);
}

void testNSArray_ObjectAtIndexedSubscriptBad(void) {
  NSArray* array = [NSArray new];
  id arg = create_tainted_id();
  NSArray* propagated = [array objectAtIndexedSubscript:arg];
  testNSArray_sink((__bridge void*)(propagated));
}

void testNSArray_ObjectAtIndexedSubscriptGood(void) {
  NSArray* array = tainted_array();
  id arg = create_untainted_id();
  [array objectAtIndexedSubscript:arg];
  testNSArray_sink(&arg);
}

void testNSArray_ObjectsAtIndexesBad(void) {
  NSArray* array = [NSArray new];
  void* arg = create_tainted();
  NSArray* propagated =
      [array objectsAtIndexes:(__bridge NSIndexSet* _Nonnull)(arg)];
  testNSArray_sink((__bridge void*)(propagated));
}

void testNSArray_ObjectsAtIndexesGood(void) {
  NSArray* array = tainted_array();
  void* arg = create_untainted();
  [array objectsAtIndexes:(__bridge NSIndexSet* _Nonnull)(arg)];
  testNSArray_sink(arg);
}

void testNSArray_ObjectEnumeratorBad(void) {
  NSArray* array = tainted_array();
  NSEnumerator* propagated = [array objectEnumerator];
  testNSArray_sink((__bridge void*)(propagated));
}

void testNSArray_ReverseObjectEnumeratorBad(void) {
  NSArray* array = tainted_array();
  NSEnumerator* propagated = [array reverseObjectEnumerator];
  testNSArray_sink((__bridge void*)(propagated));
}

void testNSArray_IndexOfObjectBad(void) {
  NSArray* array = [NSArray new];
  id arg = create_tainted_id();
  NSUInteger propagated = [array indexOfObject:arg];
  testNSArray_sink((void*)(&propagated));
}

void testNSArray_IndexOfObjectGood(void) {
  NSArray* array = tainted_array();
  id arg = create_untainted_id();
  [array indexOfObject:arg];
  testNSArray_sink(&arg);
}

void testNSArray_IndexOfObjectInRangeBad(void) {
  NSArray* array = [NSArray new];
  id arg = create_tainted_id();
  NSUInteger propagated = [array indexOfObject:arg inRange:*((NSRange*)&arg)];
  testNSArray_sink((void*)(&propagated));
}

void testNSArray_IndexOfObjectInRangeGood(void) {
  NSArray* array = tainted_array();
  id arg = create_untainted_id();
  [array indexOfObject:arg inRange:*((NSRange*)&arg)];
  testNSArray_sink(&arg);
}

void testNSArray_IndexOfObjectIdenticalToBad(void) {
  NSArray* array = [NSArray new];
  id arg = create_tainted_id();
  NSUInteger propagated = [array indexOfObjectIdenticalTo:arg];
  testNSArray_sink((void*)(&propagated));
}

void testNSArray_IndexOfObjectIdenticalToGood(void) {
  NSArray* array = tainted_array();
  id arg = create_untainted_id();
  [array indexOfObjectIdenticalTo:arg];
  testNSArray_sink(&arg);
}

void testNSArray_IndexOfObjectIdenticalToInRangeBad(void) {
  NSArray* array = [NSArray new];
  id arg = create_tainted_id();
  NSUInteger propagated = [array indexOfObjectIdenticalTo:arg
                                                  inRange:*((NSRange*)&arg)];
  testNSArray_sink((void*)(&propagated));
}

void testNSArray_IndexOfObjectIdenticalToInRangeGood(void) {
  NSArray* array = tainted_array();
  id arg = create_untainted_id();
  [array indexOfObjectIdenticalTo:arg inRange:*((NSRange*)&arg)];
  testNSArray_sink(&arg);
}

void testNSArray_IndexOfObjectPassingTestBad(void) {
  NSArray* array = [NSArray new];
  void* arg = create_tainted();
  NSUInteger propagated = [array
      indexOfObjectPassingTest:(__bridge BOOL(^_Nonnull)(id _Nonnull __strong,
                                                         NSUInteger,
                                                         BOOL* _Nonnull))(arg)];
  testNSArray_sink((void*)(&propagated));
}

void testNSArray_IndexOfObjectPassingTestGood(void) {
  NSArray* array = tainted_array();
  void* arg = create_untainted();
  [array
      indexOfObjectPassingTest:(__bridge BOOL(^_Nonnull)(id _Nonnull __strong,
                                                         NSUInteger,
                                                         BOOL* _Nonnull))(arg)];
  testNSArray_sink(arg);
}

void testNSArray_IndexOfObjectWithOptionsPassingTestBad(void) {
  NSArray* array = [NSArray new];
  void* arg = create_tainted();
  NSUInteger propagated = [array
      indexOfObjectWithOptions:arg
                   passingTest:(__bridge BOOL(^_Nonnull)(id _Nonnull __strong,
                                                         NSUInteger,
                                                         BOOL* _Nonnull))(arg)];
  testNSArray_sink((void*)(&propagated));
}

void testNSArray_IndexOfObjectWithOptionsPassingTestGood(void) {
  NSArray* array = tainted_array();
  void* arg = create_untainted();
  [array
      indexOfObjectWithOptions:arg
                   passingTest:(__bridge BOOL(^_Nonnull)(id _Nonnull __strong,
                                                         NSUInteger,
                                                         BOOL* _Nonnull))(arg)];
  testNSArray_sink(arg);
}

void testNSArray_IndexOfObjectAtIndexesOptionsPassingTestBad(void) {
  NSArray* array = [NSArray new];
  void* arg = create_tainted();
  NSUInteger propagated = [array
      indexOfObjectAtIndexes:(__bridge NSIndexSet* _Nonnull)(arg)
                     options:arg
                 passingTest:(__bridge BOOL(^_Nonnull)(id _Nonnull __strong,
                                                       NSUInteger,
                                                       BOOL* _Nonnull))(arg)];
  testNSArray_sink((void*)(&propagated));
}

void testNSArray_IndexOfObjectAtIndexesOptionsPassingTestGood(void) {
  NSArray* array = tainted_array();
  void* arg = create_untainted();
  [array
      indexOfObjectAtIndexes:(__bridge NSIndexSet* _Nonnull)(arg)
                     options:arg
                 passingTest:(__bridge BOOL(^_Nonnull)(id _Nonnull __strong,
                                                       NSUInteger,
                                                       BOOL* _Nonnull))(arg)];
  testNSArray_sink(arg);
}

void testNSArray_IndexesOfObjectsPassingTestBad(void) {
  NSArray* array = [NSArray new];
  void* arg = create_tainted();
  NSIndexSet* propagated =
      [array indexesOfObjectsPassingTest:(__bridge BOOL(^_Nonnull)(
                                             id _Nonnull __strong,
                                             NSUInteger,
                                             BOOL* _Nonnull))(arg)];
  testNSArray_sink((__bridge void*)(propagated));
}

void testNSArray_IndexesOfObjectsPassingTestGood(void) {
  NSArray* array = tainted_array();
  void* arg = create_untainted();
  [array indexesOfObjectsPassingTest:(__bridge BOOL(^_Nonnull)(
                                         id _Nonnull __strong,
                                         NSUInteger,
                                         BOOL* _Nonnull))(arg)];
  testNSArray_sink(arg);
}

void testNSArray_IndexesOfObjectsWithOptionsPassingTestBad(void) {
  NSArray* array = [NSArray new];
  void* arg = create_tainted();
  NSIndexSet* propagated =
      [array indexesOfObjectsWithOptions:arg
                             passingTest:(__bridge BOOL(^_Nonnull)(
                                             id _Nonnull __strong,
                                             NSUInteger,
                                             BOOL* _Nonnull))(arg)];
  testNSArray_sink((__bridge void*)(propagated));
}

void testNSArray_IndexesOfObjectsWithOptionsPassingTestGood(void) {
  NSArray* array = tainted_array();
  void* arg = create_untainted();
  [array indexesOfObjectsWithOptions:arg
                         passingTest:(__bridge BOOL(^_Nonnull)(
                                         id _Nonnull __strong,
                                         NSUInteger,
                                         BOOL* _Nonnull))(arg)];
  testNSArray_sink(arg);
}

void testNSArray_IndexesOfObjectsAtIndexesOptionsPassingTestBad(void) {
  NSArray* array = [NSArray new];
  void* arg = create_tainted();
  NSIndexSet* propagated = [array
      indexesOfObjectsAtIndexes:(__bridge NSIndexSet* _Nonnull)(arg)
                        options:arg
                    passingTest:(__bridge BOOL(^_Nonnull)(id _Nonnull __strong,
                                                          NSUInteger,
                                                          BOOL* _Nonnull))(
                                    arg)];
  testNSArray_sink((__bridge void*)(propagated));
}

void testNSArray_IndexesOfObjectsAtIndexesOptionsPassingTestGood(void) {
  NSArray* array = tainted_array();
  void* arg = create_untainted();
  [array indexesOfObjectsAtIndexes:(__bridge NSIndexSet* _Nonnull)(arg)
                           options:arg
                       passingTest:(__bridge BOOL(^_Nonnull)(
                                       id _Nonnull __strong,
                                       NSUInteger,
                                       BOOL* _Nonnull))(arg)];
  testNSArray_sink(arg);
}

void testNSArray_IndexOfObjectInSortedRangeOptionsUsingComparatorBad(void) {
  NSArray* array = [NSArray new];
  id arg = create_tainted_id();
  void* arg2 = create_tainted();
  NSUInteger propagated =
      [array indexOfObject:arg
             inSortedRange:*((NSRange*)&arg)options:arg
           usingComparator:(__bridge NSComparator _Nonnull)(arg2)];
  testNSArray_sink((void*)(&propagated));
}

void testNSArray_IndexOfObjectInSortedRangeOptionsUsingComparatorGood(void) {
  NSArray* array = tainted_array();
  id arg = create_untainted_id();
  void* arg2 = create_untainted();
  [array indexOfObject:arg
         inSortedRange:*((NSRange*)&arg)options:arg
       usingComparator:(__bridge NSComparator _Nonnull)(arg2)];
  testNSArray_sink(&arg);
  testNSArray_sink(arg2);
}

void testNSArray_MakeObjectsPerformSelectorBad(void) {
  NSArray* array = [NSArray new];
  void* arg = create_tainted();
  [array makeObjectsPerformSelector:arg];
  testNSArray_sink((__bridge void*)(array));
}

void testNSArray_MakeObjectsPerformSelectorGood(void) {
  NSArray* array = tainted_array();
  void* arg = create_untainted();
  [array makeObjectsPerformSelector:arg];
  testNSArray_sink(arg);
}

void testNSArray_MakeObjectsPerformSelectorWithObjectBad(void) {
  NSArray* array = [NSArray new];
  id arg = create_tainted_id();
  void* arg2 = create_tainted();
  [array makeObjectsPerformSelector:&arg2 withObject:arg];
  testNSArray_sink((__bridge void*)(array));
}

void testNSArray_MakeObjectsPerformSelectorWithObjectGood(void) {
  NSArray* array = tainted_array();
  id arg = create_untainted_id();
  void* arg2 = create_untainted();
  [array makeObjectsPerformSelector:arg2 withObject:arg];
  testNSArray_sink(&arg);
}

void testNSArray_EnumerateObjectsUsingBlockBad(void) {
  NSArray* array = [NSArray new];
  void* arg = create_tainted();
  [array enumerateObjectsUsingBlock:(__bridge void (^_Nonnull)(
                                        id _Nonnull __strong,
                                        NSUInteger,
                                        BOOL* _Nonnull))(arg)];
  testNSArray_sink((__bridge void*)(array));
}

void testNSArray_EnumerateObjectsUsingBlockGood(void) {
  NSArray* array = tainted_array();
  void* arg = create_untainted();
  [array enumerateObjectsUsingBlock:(__bridge void (^_Nonnull)(
                                        id _Nonnull __strong,
                                        NSUInteger,
                                        BOOL* _Nonnull))(arg)];
  testNSArray_sink(arg);
}

void testNSArray_EnumerateObjectsWithOptionsUsingBlockBad(void) {
  NSArray* array = [NSArray new];
  void* arg = create_tainted();
  [array enumerateObjectsWithOptions:arg
                          usingBlock:(__bridge void (^_Nonnull)(
                                         id _Nonnull __strong,
                                         NSUInteger,
                                         BOOL* _Nonnull))(arg)];
  testNSArray_sink((__bridge void*)(array));
}

void testNSArray_EnumerateObjectsWithOptionsUsingBlockGood(void) {
  NSArray* array = tainted_array();
  void* arg = create_untainted();
  [array enumerateObjectsWithOptions:arg
                          usingBlock:(__bridge void (^_Nonnull)(
                                         id _Nonnull __strong,
                                         NSUInteger,
                                         BOOL* _Nonnull))(arg)];
  testNSArray_sink(arg);
}

void testNSArray_EnumerateObjectsAtIndexesOptionsUsingBlockBad(void) {
  NSArray* array = [NSArray new];
  void* arg = create_tainted();
  [array enumerateObjectsAtIndexes:(__bridge NSIndexSet* _Nonnull)(arg)
                           options:arg
                        usingBlock:(__bridge void (^_Nonnull)(
                                       id _Nonnull __strong,
                                       NSUInteger,
                                       BOOL* _Nonnull))(arg)];
  testNSArray_sink((__bridge void*)(array));
}

void testNSArray_EnumerateObjectsAtIndexesOptionsUsingBlockGood(void) {
  NSArray* array = tainted_array();
  void* arg = create_untainted();
  [array enumerateObjectsAtIndexes:(__bridge NSIndexSet* _Nonnull)(arg)
                           options:arg
                        usingBlock:(__bridge void (^_Nonnull)(
                                       id _Nonnull __strong,
                                       NSUInteger,
                                       BOOL* _Nonnull))(arg)];
  testNSArray_sink(arg);
}

void testNSArray_FirstObjectCommonWithArrayBad(void) {
  NSArray* array = [NSArray new];
  NSArray* arg = tainted_array();
  id propagated = [array firstObjectCommonWithArray:arg];
  testNSArray_sink(&propagated);
}

void testNSArray_FirstObjectCommonWithArrayGood(void) {
  NSArray* array = tainted_array();
  NSArray* arg = [NSArray new];
  [array firstObjectCommonWithArray:arg];
  testNSArray_sink((__bridge void*)(arg));
}

void testNSArray_IsEqualToArrayBad(void) {
  NSArray* array = [NSArray new];
  NSArray* arg = tainted_array();
  BOOL propagated = [array isEqualToArray:arg];
  testNSArray_sink(&propagated);
}

void testNSArray_IsEqualToArrayGood(void) {
  NSArray* array = tainted_array();
  NSArray* arg = [NSArray new];
  [array isEqualToArray:arg];
  testNSArray_sink((__bridge void*)(arg));
}

void testNSArray_ArrayByAddingObjectBad(void) {
  NSArray* array = [NSArray new];
  id arg = create_tainted_id();
  NSArray* propagated = [array arrayByAddingObject:arg];
  testNSArray_sink((__bridge void*)(propagated));
}

void testNSArray_ArrayByAddingObjectGood(void) {
  NSArray* array = tainted_array();
  id arg = create_untainted_id();
  [array arrayByAddingObject:arg];
  testNSArray_sink((__bridge void*)(arg));
}

void testNSArray_ArrayByAddingObjectsFromArrayBad(void) {
  NSArray* array = [NSArray new];
  NSArray* arg = tainted_array();
  NSArray* propagated = [array arrayByAddingObjectsFromArray:arg];
  testNSArray_sink((__bridge void*)(propagated));
}

void testNSArray_ArrayByAddingObjectsFromArrayGood(void) {
  NSArray* array = tainted_array();
  NSArray* arg = [NSArray new];
  [array arrayByAddingObjectsFromArray:arg];
  testNSArray_sink((__bridge void*)(arg));
}

void testNSArray_FilteredArrayUsingPredicateBad(void) {
  NSArray* array = [NSArray new];
  void* arg = create_tainted();
  NSArray* propagated =
      [array filteredArrayUsingPredicate:(__bridge NSPredicate* _Nonnull)(arg)];
  testNSArray_sink((__bridge void*)(propagated));
}

void testNSArray_FilteredArrayUsingPredicateGood(void) {
  NSArray* array = tainted_array();
  void* arg = create_untainted();
  [array filteredArrayUsingPredicate:(__bridge NSPredicate* _Nonnull)(arg)];
  testNSArray_sink(arg);
}

void testNSArray_SubarrayWithRangeBad(void) {
  NSArray* array = [NSArray new];
  void* arg = create_tainted();
  NSArray* propagated = [array subarrayWithRange:*((NSRange*)arg)];
  testNSArray_sink((__bridge void*)(propagated));
}

void testNSArray_SubarrayWithRangeGood(void) {
  NSArray* array = tainted_array();
  void* arg = create_untainted();
  [array subarrayWithRange:*((NSRange*)arg)];
  testNSArray_sink(arg);
}

void testNSArray_SortedArrayHintBad(void) {
  NSArray* array = tainted_array();
  NSData* propagated = [array sortedArrayHint];
  testNSArray_sink((__bridge void*)(propagated));
}

void testNSArray_SortedArrayUsingFunctionContextBad(void) {
  NSArray* array = [NSArray new];
  void* arg = create_tainted();
  NSArray* propagated = [array sortedArrayUsingFunction:arg context:arg];
  testNSArray_sink((__bridge void*)(propagated));
}

void testNSArray_SortedArrayUsingFunctionContextGood(void) {
  NSArray* array = tainted_array();
  void* arg = create_untainted();
  [array sortedArrayUsingFunction:arg context:arg];
  testNSArray_sink(arg);
}

void testNSArray_SortedArrayUsingFunctionContextHintBad(void) {
  NSArray* array = [NSArray new];
  void* arg = create_tainted();
  NSArray* propagated =
      [array sortedArrayUsingFunction:arg
                              context:arg
                                 hint:(__bridge NSData* _Nullable)(arg)];
  testNSArray_sink((__bridge void*)(propagated));
}

void testNSArray_SortedArrayUsingFunctionContextHintGood(void) {
  NSArray* array = tainted_array();
  void* arg = create_untainted();
  [array sortedArrayUsingFunction:arg
                          context:arg
                             hint:(__bridge NSData* _Nullable)(arg)];
  testNSArray_sink(arg);
}

void testNSArray_SortedArrayUsingDescriptorsBad(void) {
  NSArray* array = [NSArray new];
  void* arg = create_tainted();
  NSArray* propagated = [array
      sortedArrayUsingDescriptors:(__bridge NSArray<
                                      NSSortDescriptor*>* _Nonnull)(arg)];
  testNSArray_sink((__bridge void*)(propagated));
}

void testNSArray_SortedArrayUsingDescriptorsGood(void) {
  NSArray* array = tainted_array();
  void* arg = create_untainted();
  [array sortedArrayUsingDescriptors:(__bridge NSArray<
                                         NSSortDescriptor*>* _Nonnull)(arg)];
  testNSArray_sink(arg);
}

void testNSArray_SortedArrayUsingSelectorBad(void) {
  NSArray* array = [NSArray new];
  void* arg = create_tainted();
  NSArray* propagated = [array sortedArrayUsingSelector:arg];
  testNSArray_sink((__bridge void*)(propagated));
}

void testNSArray_SortedArrayUsingSelectorGood(void) {
  NSArray* array = tainted_array();
  void* arg = create_untainted();
  [array sortedArrayUsingSelector:arg];
  testNSArray_sink(arg);
}

void testNSArray_SortedArrayUsingComparatorBad(void) {
  NSArray* array = [NSArray new];
  void* arg = create_tainted();
  NSArray* propagated =
      [array sortedArrayUsingComparator:(__bridge NSComparator _Nonnull)(arg)];
  testNSArray_sink((__bridge void*)(propagated));
}

void testNSArray_SortedArrayUsingComparatorGood(void) {
  NSArray* array = tainted_array();
  void* arg = create_untainted();
  [array sortedArrayUsingComparator:(__bridge NSComparator _Nonnull)(arg)];
  testNSArray_sink(arg);
}

void testNSArray_SortedArrayWithOptionsUsingComparatorBad(void) {
  NSArray* array = [NSArray new];
  void* arg = create_tainted();
  NSArray* propagated =
      [array sortedArrayWithOptions:arg
                    usingComparator:(__bridge NSComparator _Nonnull)(arg)];
  testNSArray_sink((__bridge void*)(propagated));
}

void testNSArray_SortedArrayWithOptionsUsingComparatorGood(void) {
  NSArray* array = tainted_array();
  void* arg = create_untainted();
  [array sortedArrayWithOptions:arg
                usingComparator:(__bridge NSComparator _Nonnull)(arg)];
  testNSArray_sink(arg);
}

void testNSArray_ComponentsJoinedByStringBad(void) {
  NSArray* array = [NSArray new];
  void* arg = create_tainted();
  NSString* propagated =
      [array componentsJoinedByString:(__bridge NSString* _Nonnull)(arg)];
  testNSArray_sink((__bridge void*)(propagated));
}

void testNSArray_ComponentsJoinedByStringGood(void) {
  NSArray* array = tainted_array();
  NSString* arg = [NSString new];
  [array componentsJoinedByString:arg];
  testNSArray_sink((__bridge void*)(arg));
}

void testNSArray_DescriptionBad(void) {
  NSArray* array = tainted_array();
  NSString* propagated = [array description];
  testNSArray_sink((__bridge void*)(propagated));
}

void testNSArray_DescriptionWithLocaleBad(void) {
  NSArray* array = [NSArray new];
  id arg = create_tainted_id();
  NSString* propagated = [array descriptionWithLocale:arg];
  testNSArray_sink((__bridge void*)(propagated));
}

void testNSArray_DescriptionWithLocaleGood(void) {
  NSArray* array = tainted_array();
  id arg = create_untainted_id();
  [array descriptionWithLocale:arg];
  testNSArray_sink(&arg);
}

void testNSArray_DescriptionWithLocaleIndentBad(void) {
  NSArray* array = [NSArray new];
  id arg = create_tainted_id();
  NSString* propagated = [array descriptionWithLocale:arg indent:arg];
  testNSArray_sink((__bridge void*)(propagated));
}

void testNSArray_DescriptionWithLocaleIndentGood(void) {
  NSArray* array = tainted_array();
  id arg = create_untainted_id();
  [array descriptionWithLocale:arg indent:arg];
  testNSArray_sink(&arg);
}

void testNSArray_PathsMatchingExtensionsBad(void) {
  NSArray* array = [NSArray new];
  void* arg = create_tainted();
  NSArray* propagated =
      [array pathsMatchingExtensions:(__bridge NSArray* _Nonnull)(arg)];
  testNSArray_sink((__bridge void*)(propagated));
}

void testNSArray_PathsMatchingExtensionsGood(void) {
  NSArray* array = tainted_array();
  void* arg = create_untainted();
  [array pathsMatchingExtensions:(__bridge NSArray* _Nonnull)(arg)];
  testNSArray_sink(arg);
}

void testNSArray_SetValueForKeyBad(void) {
  NSArray* array = [NSArray new];
  id arg = create_tainted_id();
  void* arg2 = create_tainted();
  [array setValue:arg forKey:(__bridge NSString* _Nonnull)(arg2)];
  testNSArray_sink((__bridge void*)(array));
}

void testNSArray_SetValueForKeyGood(void) {
  NSArray* array = tainted_array();
  id arg = create_untainted_id();
  void* arg2 = create_tainted();
  [array setValue:arg forKey:(__bridge NSString* _Nonnull)(arg2)];
  testNSArray_sink(&arg);
}

void testNSArray_ValueForKeyBad(void) {
  NSArray* array = [NSArray new];
  void* arg = create_tainted();
  id propagated = [array valueForKey:(__bridge NSString* _Nonnull)(arg)];
  testNSArray_sink(&propagated);
}

void testNSArray_ValueForKeyGood(void) {
  NSArray* array = tainted_array();
  void* arg = create_untainted();
  [array valueForKey:(__bridge NSString* _Nonnull)(arg)];
  testNSArray_sink(arg);
}

/* I fail to compile these testNSArray_NSSet_s with
 * `error: no visible @interface for 'NSArray' declares the selector
'shuffledArray'`

void testNSArray_ShuffledArrayBad(void) {
  NSArray* array = [NSArray new];
NSArray* propagated = [array shuffledArray ];
  testNSArray_sink((__bridge void*)(propagated));}

void testNSArray_ShuffledArrayWithRandomSourceBad(void) {
  NSArray* array = [NSArray new];
  void* arg = create_tainted();
NSArray* propagated = [array shuffledArrayWithRandomSource:arg ];
  testNSArray_sink((__bridge void*)(propagated));}

void testNSArray_ShuffledArrayWithRandomSourceGood(void) {
  NSArray* array = tainted_array();
  void* arg = create_untainted();
  [array shuffledArrayWithRandomSource:arg ];
  testNSArray_sink(arg);}

*/

void testNSArray_DifferenceFromArrayBad(void) {
  NSArray* array = [NSArray new];
  NSArray* arg = tainted_array();
  NSOrderedCollectionDifference* propagated = [array differenceFromArray:arg];
  testNSArray_sink((__bridge void*)(propagated));
}

void testNSArray_DifferenceFromArrayGood(void) {
  NSArray* array = tainted_array();
  NSArray* arg = [NSArray new];
  [array differenceFromArray:arg];
  testNSArray_sink((__bridge void*)(arg));
}

void testNSArray_DifferenceFromArrayWithOptionsBad(void) {
  NSArray* array = [NSArray new];
  void* arg = create_tainted();
  NSOrderedCollectionDifference* propagated =
      [array differenceFromArray:(__bridge NSArray* _Nonnull)(arg)
                     withOptions:arg];
  testNSArray_sink((__bridge void*)(propagated));
}

void testNSArray_DifferenceFromArrayWithOptionsGood(void) {
  NSArray* array = tainted_array();
  void* arg = create_untainted();
  [array differenceFromArray:(__bridge NSArray* _Nonnull)(arg) withOptions:arg];
  testNSArray_sink(arg);
}

void testNSArray_DifferenceFromArrayWithOptionsUsingEquivalenceTestBad(void) {
  NSArray* array = [NSArray new];
  void* arg = create_tainted();
  NSOrderedCollectionDifference* propagated = [array
       differenceFromArray:(__bridge NSArray* _Nonnull)(arg)
               withOptions:arg
      usingEquivalenceTest:(__bridge BOOL(^_Nonnull)(id _Nonnull __strong,
                                                     id _Nonnull __strong))(
                               arg)];
  testNSArray_sink((__bridge void*)(propagated));
}

void testNSArray_DifferenceFromArrayWithOptionsUsingEquivalenceTestGood(void) {
  NSArray* array = tainted_array();
  void* arg = create_untainted();
  [array differenceFromArray:(__bridge NSArray* _Nonnull)(arg)
                 withOptions:arg
        usingEquivalenceTest:(__bridge BOOL(^_Nonnull)(id _Nonnull __strong,
                                                       id _Nonnull __strong))(
                                 arg)];
  testNSArray_sink(arg);
}

void testNSArray_InitWithCoderBad(void) {
  NSArray* array = [NSArray new];
  void* arg = create_tainted();
  NSArray* propagated = [array initWithCoder:(__bridge NSCoder* _Nonnull)(arg)];
  testNSArray_sink((__bridge void*)(propagated));
}

void testNSArray_InitWithCoderGood(void) {
  NSArray* array = tainted_array();
  void* arg = create_untainted();
  [array initWithCoder:(__bridge NSCoder* _Nonnull)(arg)];
  testNSArray_sink(arg);
}

void testNSArray_InitWithContentsOfURLErrorBad(void) {
  NSArray* array = [NSArray new];
  void* arg = create_tainted();
  NSError* error = [NSError new];
  NSArray* propagated =
      [array initWithContentsOfURL:(__bridge NSURL* _Nonnull)(arg)
                             error:&error];
  testNSArray_sink((__bridge void*)(error));
  testNSArray_sink((__bridge void*)(propagated));
}

void testNSArray_InitWithContentsOfURLErrorGood(void) {
  NSArray* array = tainted_array();
  void* arg = create_untainted();
  NSError* error = [NSError new];
  [array initWithContentsOfURL:(__bridge NSURL* _Nonnull)(arg) error:&error];
  testNSArray_sink(arg);
}

void testNSArray_WriteToURLErrorBad(void) {
  NSArray* array = tainted_array();
  void* arg = create_untainted();
  NSError* error = [NSError new];
  BOOL propagated =
      [array writeToURL:(__bridge NSURL* _Nonnull)(arg) error:&error];
  testNSArray_sink(arg);
  testNSArray_sink((__bridge void*)(error));
  testNSArray_sink(&propagated);
}

void testNSArray_WriteToURLErrorGood(void) {
  NSArray* array = [NSArray new];
  void* arg = create_tainted();
  NSError* error = [NSError new];
  testNSArray_sink((__bridge void*)(array));
}

void testNSArray_ArrayByApplyingDifferenceBad(void) {
  NSArray* array = [NSArray new];
  void* arg = create_tainted();
  NSArray* propagated =
      [array arrayByApplyingDifference:
                 (__bridge NSOrderedCollectionDifference* _Nonnull)(arg)];
  testNSArray_sink((__bridge void*)(propagated));
}

void testNSArray_ArrayByApplyingDifferenceGood(void) {
  NSArray* array = tainted_array();
  void* arg = create_untainted();
  [array arrayByApplyingDifference:
             (__bridge NSOrderedCollectionDifference* _Nonnull)(arg)];
  testNSArray_sink(arg);
}

void testNSArray_ArrayWithContentsOfURLErrorBad(void) {
  void* arg = create_tainted();
  NSError* error = [NSError new];
  NSArray* propagated =
      [NSArray arrayWithContentsOfURL:(__bridge NSURL* _Nonnull)(arg)
                                error:&error];
  testNSArray_sink((__bridge void*)(error));
  testNSArray_sink((__bridge void*)(propagated));
}

// Stored objects with index manipulation

void NSMutableArray_insertObjectAt(NSMutableArray* mArr, void* value, int idx) {
  NSObject* obj = (__bridge NSObject*)value;
  [mArr insertObject:obj atIndex:idx];
}

NSMutableArray* init_NSMutableArray_with_tainted_and_untainted(void) {
  NSMutableArray* mArr = [NSMutableArray new];
  NSMutableArray_insertObjectAt(mArr, create_tainted(), 0);
  NSMutableArray_insertObjectAt(mArr, create_untainted(), 1);
  return mArr;
}

void* NSArray_objectAt(NSArray* arr, int idx) {
  NSObject* obj = [arr objectAtIndex:idx];
  return (__bridge void*)obj;
}

void testNSArray_cell_ObjectAtIndex_bad(void) {
  NSMutableArray* mArr = init_NSMutableArray_with_tainted_and_untainted();
  void* value = NSArray_objectAt(mArr, 0);
  testNSArray_sink(value);
}

void testNSArray_cell_ObjectAtIndex_good(void) {
  NSMutableArray* mArr = init_NSMutableArray_with_tainted_and_untainted();
  void* value = NSArray_objectAt(mArr, 1);
  testNSArray_sink(value);
}

void* NSArray_objectAtIndexedSubscript(NSArray* arr, int idx) {
  NSObject* obj = arr[idx];
  return (__bridge void*)obj;
}

void testNSArray_cell_ObjectAtIndexedSubscript_bad(void) {
  NSMutableArray* mArr = init_NSMutableArray_with_tainted_and_untainted();
  void* value = NSArray_objectAtIndexedSubscript(mArr, 0);
  testNSArray_sink(value);
}

void testNSArray_cell_ObjectAtIndexedSubscript_good(void) {
  NSMutableArray* mArr = init_NSMutableArray_with_tainted_and_untainted();
  void* value = NSArray_objectAtIndexedSubscript(mArr, 1);
  testNSArray_sink(value);
}

void NSArray_replaceObjectAtIndexWithObject(NSMutableArray* mArr,
                                            int idx,
                                            void* value) {
  NSObject* obj = (__bridge NSObject*)value;
  [mArr replaceObjectAtIndex:idx withObject:obj];
}

void testNSArray_cell_ReplaceObjectAtIndexWithObject_bad(void) {
  NSMutableArray* mArr = init_NSMutableArray_with_tainted_and_untainted();
  NSArray_replaceObjectAtIndexWithObject(mArr, 1, create_tainted());
  void* value = NSArray_objectAt(mArr, 1);
  testNSArray_sink(value);
}

void testNSArray_cell_ReplaceObjectAtIndexWithObject_good(void) {
  NSMutableArray* mArr = init_NSMutableArray_with_tainted_and_untainted();
  NSArray_replaceObjectAtIndexWithObject(mArr, 0, create_untainted());
  void* value = NSArray_objectAt(mArr, 0);
  testNSArray_sink(value);
}

void NSArray_setObjectAtIndexedSubscript(NSMutableArray* mArr,
                                         void* value,
                                         int idx) {
  NSObject* obj = (__bridge NSObject*)value;
  mArr[idx] = obj;
}

void testNSArray_cell_SetObjectAtIndexedSubscript_bad(void) {
  NSMutableArray* mArr = init_NSMutableArray_with_tainted_and_untainted();
  NSArray_setObjectAtIndexedSubscript(mArr, create_tainted(), 1);
  void* value = NSArray_objectAt(mArr, 1);
  testNSArray_sink(value);
}

void testNSArray_cell_SetObjectAtIndexedSubscript_good(void) {
  NSMutableArray* mArr = init_NSMutableArray_with_tainted_and_untainted();
  NSArray_setObjectAtIndexedSubscript(mArr, create_untainted(), 0);
  void* value = NSArray_objectAt(mArr, 0);
  testNSArray_sink(value);
}

void testNSArray_cell_ArrayWithArrayBad(void) {
  NSMutableArray* mArr = init_NSMutableArray_with_tainted_and_untainted();
  NSArray* copied = [NSArray arrayWithArray:mArr];
  NSArray_setObjectAtIndexedSubscript(mArr, create_untainted(), 0);
  void* value = NSArray_objectAt(copied, 0);
  testNSArray_sink(value);
}

void testNSArray_cell_ArrayWithArrayGood(void) {
  NSMutableArray* mArr = init_NSMutableArray_with_tainted_and_untainted();
  NSArray* copied = [NSArray arrayWithArray:mArr];
  NSArray_setObjectAtIndexedSubscript(mArr, create_tainted(), 1);
  void* value = NSArray_objectAt(copied, 1);
  testNSArray_sink(value);
}

void sinkObjectAt0(NSMutableArray* mArr) {
  void* value = NSArray_objectAt(mArr, 0);
  testNSArray_sink(value);
}

void sinkObjectAt1(NSMutableArray* mArr) {
  void* value = NSArray_objectAt(mArr, 1);
  testNSArray_sink(value);
}

void testNSArray_cell_sinkInCalleeBad(void) {
  NSMutableArray* mArr = init_NSMutableArray_with_tainted_and_untainted();
  sinkObjectAt0(mArr);
}

void testNSArray_cell_sinkInCalleeGood(void) {
  NSMutableArray* mArr = init_NSMutableArray_with_tainted_and_untainted();
  sinkObjectAt1(mArr);
}
