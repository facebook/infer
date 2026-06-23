/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

/* FP case present in LibXML */
struct _xmlLink {
  struct _xmlLink* next;
  struct _xmlLink* prev;
  void* data;
};

typedef struct _xmlLink* xmlLinkPtr;

struct _xmlList {
  xmlLinkPtr sentinel;
  void (*linkDeallocator)(xmlLinkPtr);
  int (*linkCompare)(const void*, const void*);
};

typedef struct _xmlList xmlList;

/* Found in Libxml : no bug */
int FP_xmlListSize_ok(xmlList* l) {
  xmlLinkPtr lk;
  int count = 0;

  if (l == 0)
    return (-1);
  for (lk = l->sentinel->next; lk != l->sentinel; lk = lk->next, count++)
    ;
  return count;
}
