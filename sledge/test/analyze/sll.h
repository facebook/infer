/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

typedef struct sll_entry {
  int data;
  struct sll_entry* next;
} sll_entry, *sll_head;

sll_head
sll_cons(int data, sll_head head)
{
  sll_head tmp = (sll_head)malloc(sizeof(sll_entry));
  if (tmp == NULL)
    return NULL;
  tmp->data = data;
  tmp->next = head;
  return tmp;
}

sll_head
sll_create(int length)
{
  sll_head head = NULL;
  sll_head tmp;
  for (int i = 0; i < length; i++) {
    tmp = (sll_head)malloc(sizeof(sll_entry));
    if (tmp == NULL)
      return head;
    tmp->next = head;
    head = tmp;
  }
  return head;
}

void
sll_destroy(sll_head head)
{
  sll_head tmp;
  while (head != NULL) {
    tmp = head;
    head = head->next;
    free(tmp);
  }
}
