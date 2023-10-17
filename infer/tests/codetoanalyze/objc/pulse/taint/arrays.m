/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/Foundation.h>

int int_source(void);
void sink_int(int);
void sink_number(NSNumber*) {}

void test_intra_proc_bad(void) {
  int arr[2];
  arr[0] = int_source();
  sink_int(arr[0]);
}

void test_intra_proc_good(void) {
  int arr[2];
  arr[0] = int_source();
  sink_int(arr[1]);
}

void test_intra_change_0_bad(int i) {
  int arr[2];
  arr[0] = int_source();
  if (i == 0)
    sink_int(arr[i]);
}

void test_intra_change_0_good(int i) {
  int arr[2];
  arr[0] = int_source();
  if (i == 0)
    sink_int(arr[1]);
}

void taint_cell(int* arr, int idx) { arr[idx] = int_source(); }

void sink_cell(int* arr, int idx) { sink_int(arr[idx]); }

void test_inter_proc_bad(void) {
  int arr[2];
  taint_cell(arr, 0);
  sink_cell(arr, 0);
}

void test_inter_proc_good(void) {
  int arr[2];
  taint_cell(arr, 0);
  sink_cell(arr, 1);
}

void test_intra_NSMutableArray_bad(void) {
  NSMutableArray* mArr = [NSMutableArray new];
  NSNumber* obj = [NSNumber numberWithInt:int_source()];
  [mArr insertObject:obj atIndex:0];
  [mArr insertObject:@42 atIndex:1];
  NSNumber* obj2 = [mArr objectAtIndex:0];
  sink_number(obj2);
}

void test_intra_NSMutableArray_good(void) {
  NSMutableArray* mArr = [NSMutableArray new];
  NSNumber* obj = [NSNumber numberWithInt:int_source()];
  [mArr insertObject:obj atIndex:0];
  [mArr insertObject:@42 atIndex:1];
  NSNumber* obj2 = [mArr objectAtIndex:1];
  sink_number(obj2);
}

void taint_NSMutableArray_cell(NSMutableArray* mArr, int idx) {
  NSNumber* obj = [NSNumber numberWithInt:int_source()];
  [mArr insertObject:obj atIndex:idx];
}

void sink_NSMutableArray_cell(NSMutableArray* mArr, int idx) {
  NSNumber* obj = [mArr objectAtIndex:idx];
  sink_number(obj);
}

void test_inter_NSMutableArray_bad(void) {
  NSMutableArray* mArr = [NSMutableArray new];
  taint_NSMutableArray_cell(mArr, 0);
  [mArr insertObject:@42 atIndex:1];
  sink_NSMutableArray_cell(mArr, 0);
}

void test_inter_NSMutableArray_good(void) {
  NSMutableArray* mArr = [NSMutableArray new];
  taint_NSMutableArray_cell(mArr, 0);
  [mArr insertObject:@42 atIndex:1];
  sink_NSMutableArray_cell(mArr, 1);
}
