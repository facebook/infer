/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "sll.h"

sll_head
interleave(sll_head head1, sll_head head2)
{
  if (head1 == NULL)
    return head2;
  if (head2 == NULL)
    return head1;
  sll_head prev = head1;
  sll_head curr = head2;
  while (1) {
    sll_head temp = curr;
    curr = curr->next;
    temp->next = prev->next;
    prev->next = temp;
    prev = temp->next;
    if (curr == NULL)
      return head1;
    if (prev == NULL) {
      temp->next = curr;
      return head1;
    }
  }
}

void
main()
{
  sll_head x, y, z;
  x = sll_create(__llair_choice());
  y = sll_create(__llair_choice());
  z = interleave(x, y);
  sll_destroy(z);
}
