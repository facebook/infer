/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "my_typedef.h"

struct st {
  int field;
};

int get_field(t* x) { return x->field; }

void call_get_field_cond_Bad() {
  int a[5];
  t x = {0};
  if (get_field_wrapper(&x)) {
    a[10] = 0;
  } else {
    a[10] = 0;
  }
}

void call_get_field_Good() {
  int a[5];
  t x = {0};
  a[get_field_wrapper(&x)] = 0;
}

void call_get_field_Bad() {
  int a[5];
  t x = {10};
  a[get_field_wrapper(&x)] = 0;
}

struct List {
  struct List* next;
  struct List* prev;
  int v;
};

int get_v(struct List* l) { return l->next->prev->v; }

int call_get_v_Good() {
  int a[10];
  struct List* l = (struct List*)malloc(sizeof(struct List));
  struct List* next = (struct List*)malloc(sizeof(struct List));
  l->next = next;
  next->prev = l;
  l->v = 0;
  next->v = 10;
  a[get_v(l)] = 0;
}

int call_get_v_Bad() {
  int a[10];
  struct List* l = (struct List*)malloc(sizeof(struct List));
  struct List* next = (struct List*)malloc(sizeof(struct List));
  l->next = next;
  next->prev = l;
  l->v = 10;
  next->v = 0;
  a[get_v(l)] = 0;
}

// [l->next->prev->next->prev->v] is abstracted in [l->next->prev.*.v].
int get_v2(struct List* l) { return l->next->prev->next->prev->v; }

int call_get_v2_Good_FP() {
  int a[10];
  struct List* l = (struct List*)malloc(sizeof(struct List));
  struct List* next = (struct List*)malloc(sizeof(struct List));
  l->next = next;
  next->prev = l;
  l->v = 0;
  next->v = 10;
  a[get_v2(l)] = 0;
}

int call_get_v2_Bad() {
  int a[10];
  struct List* l = (struct List*)malloc(sizeof(struct List));
  struct List* next = (struct List*)malloc(sizeof(struct List));
  l->next = next;
  next->prev = l;
  l->v = 10;
  next->v = 0;
  a[get_v2(l)] = 0;
}

struct t1 {
  struct t1* a;
  struct t1* b;
  struct t1* c;
  struct t1* d;
  struct t1* e;
  struct t1* f;
  struct t1* g;
  struct t1* h;
  struct t1* i;
  struct t1* j;
};

int unknown;

// The analysis should be terminated within a reasonable amount of time.
void make_many_locations(struct t1* x) {
  while (1) {
    switch (unknown) {
      case 0:
        x = x->a;
        break;
      case 1:
        x = x->b;
        break;
      case 2:
        x = x->c;
        break;
      case 3:
        x = x->d;
        break;
      case 4:
        x = x->e;
        break;
      case 5:
        x = x->f;
        break;
      case 6:
        x = x->g;
        break;
      case 7:
        x = x->h;
        break;
      case 8:
        x = x->i;
        break;
      default:
        x = x->j;
        break;
    }
  }
}
