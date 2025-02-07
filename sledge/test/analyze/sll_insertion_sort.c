/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "sll.h"

sll_head
insert(sll_head sorted, sll_head item)
{
  sll_head curr = sorted;
  sll_head prev = NULL;
  while (curr != NULL) {
    if (curr->data >= item->data) {
      item->next = curr;
      if (prev == NULL)
        return item;
      prev->next = item;
      return sorted;
    }
    prev = curr;
    curr = curr->next;
  }
  item->next = curr;
  if (prev == NULL)
    return item;
  prev->next = item;
  return sorted;
}

sll_head
insertion_sort(sll_head head)
{
  sll_head sorted = NULL;
  sll_head item;
  while (head != NULL) {
    item = head;
    head = head->next;
    item->next = NULL;
    sorted = insert(sorted, item);
  }
  return sorted;
}

int
main()
{
  sll_head head;
  head = sll_create(__llair_choice());
  head = insertion_sort(head);
  sll_destroy(head);
  return 0;
}
