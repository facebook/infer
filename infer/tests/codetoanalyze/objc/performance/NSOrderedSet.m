/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/Foundation.h>

void nsordered_set_iterate_linear(NSOrderedSet* ordered_set) {
  for (id item in ordered_set) {
  }
}

void nsordered_set_empty_constant() {
  NSOrderedSet* ordered_set = [NSOrderedSet orderedSet];
  nsordered_set_iterate_linear(ordered_set);
}

void nsordered_set_with_array_linear(NSArray* array) {
  NSOrderedSet* ordered_set = [NSOrderedSet orderedSetWithArray:array];
  nsordered_set_iterate_linear(ordered_set);
}
