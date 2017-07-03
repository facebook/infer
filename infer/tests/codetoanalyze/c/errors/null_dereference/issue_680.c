/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
#include <stdlib.h>

struct list {
  struct list* next;
};

void go_to_next(struct list* head) {
  if (head->next != NULL) {
    head = head->next;
  }
}

void null_ptr_deref_bad() { go_to_next(NULL); }

void go_to_end_of_list(struct list* head) {
  while (head->next != NULL) {
    head = head->next;
  }
}

void null_ptr_deref_bad_FN() { go_to_end_of_list(NULL); }

void check_next(struct list* head) {
  while (head->next != NULL) {
    whatever();
  }
}

void null_ptr_deref2_bad() { check_next(NULL); }
