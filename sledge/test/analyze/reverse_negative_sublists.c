/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "sll.h"

void
reverse_negative_sublists(sll_head* prev)
{
  sll_head curr = *prev;
  while (curr != NULL) {
    while (curr != NULL && curr->data >= 0) {
      prev = &(curr->next);
      curr = *prev;
    }
    if (curr != NULL) {
      sll_head rev_head = curr;
      sll_head* rev_tail = &(curr->next);
      do {
        sll_head temp = curr;
        curr = curr->next;
        temp->next = rev_head;
        rev_head = temp;
      } while (curr != NULL && curr->data < 0);
      *rev_tail = curr;
      *prev = rev_head;
    }
  }
}

sll_head
reverse_negative_sublists_simple(sll_head curr)
{
  sll_head head = sll_cons(0, curr);
  if (head == NULL)
    return curr;
  sll_head prev = head;
  while (curr != NULL) {
    while (curr != NULL && curr->data >= 0) {
      prev = curr;
      curr = curr->next;
    }
    if (curr != NULL) {
      sll_head rev_head = curr;
      sll_head rev_tail = curr;
      do {
        sll_head temp = curr;
        curr = curr->next;
        temp->next = rev_head;
        rev_head = temp;
      } while (curr != NULL && curr->data < 0);
      rev_tail->next = curr;
      prev->next = rev_head;
    }
  }
  sll_head temp = head->next;
  free(head);
  return temp;
}

int
main()
{
  sll_head head = sll_create(__llair_choice());
  if (head == NULL)
    return 0;
  head = reverse_negative_sublists_simple(head);
  reverse_negative_sublists(&head);
  sll_destroy(head);
  return 0;
}
