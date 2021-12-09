/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <stdlib.h>

typedef struct Point {
  int x;
  int y;
} Point;

int return_5() { return 5; }

void init_Point() { struct Point p = {1, return_5() + 3}; }

int point_coords_set_correctly_npe_bad(Point* p) {
  *p = (Point){4, 5};
  int* pointer = NULL;
  if (p->x == 4) {
    return *pointer;
  } else
    return 0;
}

int point_coords_set_correctly_npe_good(Point* p) {
  *p = (Point){4, 5};
  int* pointer = NULL;
  if (p->x == 1) {
    return *pointer;
  } else
    return 0;
}

struct Employee {
  int ssn;
  float salary;
  struct date {
    int date;
    int month;
    int year;
  } doj;
} emp1;

int field_set_correctly_npe_bad() {
  struct Employee e = {12, 3000.50, 12, 12, 2010};
  int* p = NULL;
  if (e.ssn == 12) {
    return *p;
  } else
    return 0;
}

int field_set_correctly_npe_good() {
  struct Employee e = {12, 3000.50, 12, 12, 2010};
  int* p = NULL;
  if (e.ssn == 1) {
    return *p;
  } else
    return 0;
}

struct dotdot {
  int a;
  int b;
};

struct dot {
  struct dotdot x;
  int y;
};
struct rect {
  struct dot origin;
  int z;
  int size;
};

typedef struct rect rect;

int implicit_expr_set_correctly_npe_bad() {
  rect imageDrawRect;
  imageDrawRect = (rect){.size = 5};
  int* p = NULL;
  if (imageDrawRect.origin.x.a == 0) {
    return *p;
  } else
    return 0;
}

int implicit_expr_set_correctly_npe_good() {
  rect imageDrawRect;
  imageDrawRect = (rect){.size = 5};
  int* p = NULL;
  if (imageDrawRect.origin.x.a == 1) {
    return *p;
  } else
    return 0;
}
