/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/Foundation.h>

// init array

void nsarray_empty_array_constant() {
  NSArray* array = [NSArray array];

  for (int i = 0; i < array.count; i++) {
  }
}

void nsarray_init_constant() {
  NSArray* array = [[NSArray alloc] init];

  for (int i = 0; i < array.count; i++) {
  }
}

void nsarray_init_with_array_linear_FP(NSArray* array) {
  NSArray* ref_array = [[NSArray alloc] initWithArray:array];

  for (int i = 0; i < ref_array.count; i++) {
  }
}

void nsarray_init_with_array_constant_FP() {
  NSArray* array = [[NSArray alloc] init];
  nsarray_init_with_array_linear_FP(array);
}

void nsarray_init_with_array_copy_linear_FP(NSArray* array) {
  NSArray* copy_array = [[NSArray alloc] initWithArray:array copyItems:YES];
  for (int i = 0; i < copy_array.count; i++) {
  }
}

NSArray* nsarray_init_with_objects_constant() {
  NSString* strings[3];
  strings[0] = @"First";
  strings[1] = @"Second";
  strings[2] = @"Third";

  NSArray* array = [NSArray arrayWithObjects:strings count:2];
  for (int i = 0; i < array.count; i++) {
  }
}

NSArray* nsarray_array_with_objects_constant() {
  NSDate* aDate = [NSDate distantFuture];
  NSValue* aValue = @(5);
  NSString* aString = @"hello";

  NSArray* array = [NSArray arrayWithObjects:aDate, aValue, aString, nil];
  for (id item in array) {
  }

  return array;
}

// derive new array

NSArray* nsarray_add_object_constant(id obj) {
  NSArray* array = [[NSArray alloc] init];
  return [array arrayByAddingObject:obj];
}

NSArray* nsarray_add_objects_from_array_linear_FN(NSArray* append_array) {
  NSArray* array = [[NSArray alloc] init];
  return [array arrayByAddingObjectsFromArray:append_array];
}

// query element

void nsarray_access_constant() {
  NSArray* array = @[ @1.0f, @2.0f, @3.0f, @4.0f, @5.0f, @6.0f, @7.0f, @8.0f ];
  for (int i = 0; i < 4; i++) {
    [array objectAtIndex:i];
  }
}

void nsarray_object_at_indexed_constant_FP() {
  NSArray* array = @[ @2, @3 ];

  for (int i = 0; i < [array[0] integerValue]; i++) {
  }
}

void nsarray_access_linear(NSArray* array) {
  id obj;
  for (int i = 0; i < array.count; i++) {
    obj = array[i];
  }
}

void nsarray_find_linear(NSArray* array) {
  for (int i = 0; i < array.count && i != [array[i] integerValue]; i++) {
  }
}

void nsarray_contains_object_linear_FN(NSArray* array) {
  [array containsObject:@1];
}

id nsarray_first_object_constant(NSArray* array) { return array.firstObject; }

id nsarray_last_object_constant(NSArray* array) { return array.lastObject; }

// find element
NSInteger nsarray_binary_search_log_FN(NSArray* sorted_array) {
  NSNumber* target = @5;
  return [sorted_array indexOfObject:target
                       inSortedRange:NSMakeRange(0, sorted_array.count)
                             options:NSBinarySearchingFirstEqual
                     usingComparator:^(id lhs, id rhs) {
                       return [lhs compare:rhs];
                     }];
}

// sort array

NSArray* nsarray_sort_using_descriptors_constant() {
  NSArray* array = @[ @"Grapes", @"Apples", @"Oranges" ];
  NSSortDescriptor* sd = [[NSSortDescriptor alloc] initWithKey:nil
                                                     ascending:YES];
  return [array sortedArrayUsingDescriptors:@[ sd ]];
}

NSArray* nsarray_sort_using_descriptors_nlogn_FN(NSArray* array) {
  NSSortDescriptor* sd = [[NSSortDescriptor alloc] initWithKey:nil
                                                     ascending:YES];
  return [array sortedArrayUsingDescriptors:@[ sd ]];
}

// iterate through array

void nsarray_iterate_linear_FN(NSArray* array) {
  NSInteger sum = 0;
  for (id obj in array) {
    sum += (NSInteger)obj;
  }
}

void nsarray_enumerator_linear_FN(NSArray* array) {
  NSEnumerator* enumerator = [array objectEnumerator];

  id obj;
  NSInteger sum = 0;

  while (obj = [enumerator nextObject]) {
    sum += (NSInteger)obj;
  }
}

void nsarray_next_object_linear_FN(NSArray* array) {
  for (id item in array) {
  }
}

// compare array
boolean_t nsarray_is_equal_to_array_linear_FN(NSArray* array1,
                                              NSArray* array2) {
  return [array1 isEqualToArray:array2];
}

// count

void nsarray_count_bounded_linear(NSArray* array) {
  for (int i = 0; i < array.count; i++) {
  }
}
