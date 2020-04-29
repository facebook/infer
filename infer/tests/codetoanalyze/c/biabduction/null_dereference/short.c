/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

struct data {
  int flag;
};

static struct data d;

int ternary1_ok(int x) {
  struct data* p = x ? &d : 0;
  return (p && p->flag);
}

int ternary2_bad(int x) {
  struct data* p = x ? &d : 0;
  return p->flag && p; // NULL_DEREF
}

int ternary3_ok(int x) {
  struct data* p = x ? &d : 0;
  return !(p && p->flag); // OK
}

int ternary4_ok(int x) {
  struct data* p = x ? &d : 0;
  return !(p && p->flag); // OK
}

int ternary4_bad(int x) {
  struct data* p = x ? &d : 0;
  return !(p->flag && p); // NULL_DEREF
}

int ternary5_ok(int x) {
  struct data* p = x ? &d : 0;
  return !(p && p->flag); // OK
}

int ternary6_ok(int x) {
  struct data* p = x ? &d : 0;
  return !p || p->flag; // OK
}

int ternary7_bad(int x) {
  struct data* p = x ? &d : 0;
  return p || p->flag; // NULL_DEREF
}

int ternary8_ok(int x) {
  struct data* p = x ? &d : 0;
  return (!p || p->flag) && !(p && p->flag); // OK
}

int ternary9_ok(int x) {
  struct data* p = x ? &d : 0;
  return p && (p->flag || !(p->flag)); // OK
}
