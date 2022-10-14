/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <stdlib.h>
#include <stdint.h>

void simple_deref() {
  int* p = NULL;
  int* q = p; // this line should not be part of the trace
  *p = 42;
}

void simple_deref_via_alias() {
  int* p = NULL;
  int* q = p;
  *q = 42;
}

void something_about_strings_latent(const uint16_t* utf16String,
                                    size_t utf16StringLen,
                                    uint8_t** utf8StringOut,
                                    size_t* utf8StringLenOut) {
  if (!utf16String || utf16StringLen < 0) {
    *utf8StringOut = NULL;
    return;
  }

  if (utf16StringLen == 0) {
    // forgot the null check here
    *utf8StringOut = (uint8_t*)malloc(1);
    **utf8StringOut = '\0'; // the trace should be just the calling context +
                            // malloc + this line
    *utf8StringLenOut = 0;
    return;
  }
}

void call_makes_null_deref_manifest_bad() {
  uint16_t utf16String;
  uint8_t* utf8String;
  size_t utf8StringLen;
  something_about_strings_latent(&utf16String, 0, &utf8String, &utf8StringLen);
  free(utf8String);
}

struct list {
  struct list* next;
  int data;
};

void access_null_deref_bad() {
  struct list l = {NULL, 44};
  l.next->next = NULL;
}

void access_use_after_free_bad(struct list* l) {
  free(l->next);
  l->next->next = NULL;
}
