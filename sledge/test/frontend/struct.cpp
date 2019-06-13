/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <stdlib.h>

struct list_head {
  struct list_head *next, *prev;
};

struct list_head glob = {0};

struct list_head ears = {&ears, &ears};

int main() {
  struct list_head* node = (struct list_head*)malloc(sizeof(struct list_head));
  node->next = node->prev = node;
  free(node);
  if (&glob != &ears)
    return 1;
  return 0;
}
