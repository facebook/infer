/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/Foundation.h>

@class Intptr;

@interface Intptr : NSObject

@property int i;

@end

@implementation Intptr
@end

void incr_int_deref(int* x, int* y) {
  (*x)++;
  (*y)++;
}

void incr_int_field(Intptr* x, Intptr* y) {
  x.i++;
  y.i++;
}

void call_incr_int_deref_with_alias_bad(void) {
  int x = 0;
  int* ptr = &x;
  incr_int_deref(ptr, ptr);
  if (x == 2) {
    ptr = NULL;
  }
  x = *ptr;
}

void call_incr_int_deref_with_alias_good(void) {
  int x = 0;
  int* ptr = &x;
  incr_int_deref(ptr, ptr);
  if (x != 2) {
    ptr = NULL;
  }
  x = *ptr;
}

void call_incr_int_field_with_alias_bad(void) {
  Intptr* ip = [Intptr new];
  ip.i = 0;
  incr_int_field(ip, ip);
  int x = ip.i;
  int* ptr = &x;
  if (ip.i == 2) {
    ptr = NULL;
  }
  ip.i = *ptr;
}

void call_incr_int_field_with_alias_good(void) {
  Intptr* ip = [Intptr new];
  ip.i = 0;
  incr_int_field(ip, ip);
  int x = ip.i;
  int* ptr = &x;
  if (ip.i != 2) {
    ptr = NULL;
  }
  ip.i = *ptr;
}
