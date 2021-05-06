/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "ref_stack.h"
#include <assert.h>
#include <stdlib.h>

ref_stack* new_ref_stack(int max_size) {
  ref_stack* result = malloc(sizeof(ref_stack));
  result->data = malloc(sizeof(data_type) * max_size);
  result->max_size = max_size;
  result->size = 0;
  return result;
}

void ref_stack_push(ref_stack* s, data_type v) {
  assert(s != NULL);
  assert(s->size < s->max_size);
  s->data[s->size] = v;
  ++s->size;
}

data_type ref_stack_pop(ref_stack* s) {
  assert(s != NULL);
  assert(s->size > 0);
  --s->size;
  data_type result = s->data[s->size];
  return result;
}
