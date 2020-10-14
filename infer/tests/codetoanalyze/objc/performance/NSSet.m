/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/Foundation.h>

// init set

void nsset_init_constant() {
  NSSet* set = [[NSSet alloc] init];

  for (int i = 0; i < set.count; i++) {
  }
}

void nsset_init_with_set_constant() {
  NSSet* set = [[NSSet alloc] init];
  nsset_init_with_set_linear(set);
}

void nsset_init_with_array_linear(NSArray* array) {
  NSSet* ref_set = [[NSSet alloc] initWithArray:array];

  for (int i = 0; i < ref_set.count; i++) {
  }
}

// iterate through set

void nsset_iterate_linear(NSSet* set) {
  NSInteger sum = 0;
  for (id obj in set) {
    sum += (NSInteger)obj;
  }
}

void nsset_enumerator_linear(NSSet* set) {
  NSEnumerator* enumerator = [set objectEnumerator];

  id obj;
  NSInteger sum = 0;

  while (obj = [enumerator nextObject]) {
    sum += (NSInteger)obj;
  }
}

void nsset_next_object_linear(NSSet* set) {
  for (id item in set) {
  }
}
