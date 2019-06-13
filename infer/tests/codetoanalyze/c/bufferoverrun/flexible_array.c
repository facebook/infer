/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <stdlib.h>

struct s {
  int useless_field_because_flexible_array_members_are_not_allowed_in_otherwise_empty_struct;
  int arr[];
};

void access_to_incomplete_array_type(struct s* a, int i) { a->arr[i] = 0; }

void call_access_to_incomplete_array_type_Good() {
  struct s* x = malloc(sizeof(struct s) + sizeof(int));
  access_to_incomplete_array_type(x, 0);
}

void call_access_to_incomplete_array_type_Bad_FN() {
  struct s* x = malloc(sizeof(struct s) + sizeof(int));
  access_to_incomplete_array_type(x, 1);
}

// For tests with last field of size 0 or 1, see class.cpp
