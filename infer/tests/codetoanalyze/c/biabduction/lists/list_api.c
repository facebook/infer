/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <stdlib.h>

typedef struct list_elem_s {
  void* data;
  struct list_elem_s* next;
} list_elem_t;

typedef struct list_s {
  list_elem_t* first;
} list_t;

list_t* list_init() { return calloc(1, sizeof(list_t)); }

void list_append(list_t* lst, list_elem_t* elem) {
  list_elem_t* iter;

  if (NULL == lst->first) {
    lst->first = elem;
  } else {
    iter = lst->first;
    while (NULL != iter->next) {
      iter = iter->next;
    }
    iter->next = elem;
  }
}

list_elem_t* list_add(list_t* lst, void* data) {
  list_elem_t* entry;

  entry = calloc(1, sizeof(list_elem_t));
  if (NULL == entry) {
    return NULL;
  }
  entry->data = data;
  list_append(lst, entry);
  return (entry);
}

void list_elem_free(list_elem_t* ptr) {
  if (NULL == ptr) {
    return;
  }

  list_elem_free(ptr->next);
  free(ptr);
}

void list_free(list_t* ptr) {
  list_elem_free(ptr->first);
  free(ptr);
}

int FP_list_build_and_free_good() {
  int val_data = 21;

  list_t* list = list_init();
  if (NULL == list) {
    return 1;
  }
  if (NULL == list_add(list, &val_data)) {
    list_free(list);
    return 1;
  }
  list_free(list);
  return 0;
}
