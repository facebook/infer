/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/Foundation.h>

// init array

void nsmarray_init_with_capacity_constant() {
  NSMutableArray* table = [[NSMutableArray alloc] initWithCapacity:256];

  for (int i = 0; i < table.count; i++) {
    table[i] = @"somevalue";
  }
}

// add element

void nsmarray_empty_ok_costant() {
  NSMutableArray* array = [[NSMutableArray alloc] init];
  [array insertObject:@1 atIndex:0];
}

void nsmarray_add_in_loop_constant() {
  NSMutableArray* array = [[NSMutableArray alloc] init];
  for (int i = 0; i < 10; i++) {
    [array addObject:[NSNumber numberWithInt:i]];
  }
  for (int i = 0; i < array.count; i++) {
  }
}

void nsmarray_add_in_loop_linear(NSUInteger n) {
  NSMutableArray* array = [[NSMutableArray alloc] init];
  for (int i = 0; i < n; i++) {
    [array addObject:[NSNumber numberWithInt:i]];
  }
  for (int i = 0; i < array.count; i++) {
  }
}

void nsmarray_add_in_loop_quadratic(NSUInteger n, NSUInteger m) {
  NSMutableArray* array = [[NSMutableArray alloc] init];
  for (int i = 0; i < n; i++) {
    for (int j = 0; j < m; j++) {
      [array addObject:[NSNumber numberWithInt:i + j]];
    }
  }
}

void nsmarray_add_then_loop_constant() {
  NSMutableArray* array = [[NSMutableArray alloc] init];
  [array addObject:@0];
  [array addObject:@1];
  [array addObject:@2];
  [array addObject:@3];
  [array addObject:@4];
  [array addObject:@5];
  [array addObject:@6];
  [array addObject:@7];
  [array addObject:@8];
  [array addObject:@9];
  [array addObject:@10];

  for (int i = 0; i < array.count; i++) {
  }
}

void nsmarray_add_all_constant() {
  NSMutableArray* array1 = [[NSMutableArray alloc] init];
  [array1 addObject:@2];
  [array1 addObject:@3];

  NSMutableArray* array2 = [[NSMutableArray alloc] init];
  [array2 addObject:@0];
  [array2 addObject:@1];

  [array2 addObjectsFromArray:array1];
}

// set element

void nsmarray_set_linear(NSMutableArray* array) {
  for (int i = 0; i < array.count; i++) {
    array[i] = [NSNumber numberWithInt:([array[i] intValue] + 1)];
  }
}

void nsmarray_set_in_loop_constant() {
  NSMutableArray* array = [[NSMutableArray alloc] init];
  [array addObject:@0];
  [array addObject:@1];
  [array addObject:@2];

  for (int i = 0; i < array.count; i++) {
    array[i] = [NSNumber numberWithInt:i];
  }
}

// remove element

id nsmarray_remove_constant() {
  NSMutableArray* array = [[NSMutableArray alloc] init];
  [array addObject:@0];
  [array addObject:@1];
  [array removeObjectAtIndex:0];
  [array removeLastObject];
  return array[0];
}

void nsmarray_remove_in_loop_constant() {
  NSMutableArray* array = [[NSMutableArray alloc] init];
  for (int i = 0; i < 10; i++) {
    [array addObject:[NSNumber numberWithInt:i]];
  }
  for (int i = 0; i < array.count; i++) {
    [array removeObjectAtIndex:i];
  }
}

void nsmarray_remove_all_linear(NSMutableArray* array) {

  [array removeAllObjects];
  for (int i = 0; i < array.count; i++) {
  }
}

void nsarray_new_constant(int x) {
  NSMutableArray* arr = [NSMutableArray new];
  for (int i = 0; i < arr.count; i++) {
  }
}
