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

int f(int x) {
  struct data* p = x ? &d : 0;
  return (p && p->flag);
}

int f_error(int x) {
  struct data* p = x ? &d : 0;
  return p->flag && p; // NULL_DEREF
}

int g(int x) {
  struct data* p = x ? &d : 0;
  return !(p && p->flag); // OK
}

int h(int x) {
  struct data* p = x ? &d : 0;
  return !(p && p->flag); // OK
}

int g_error(int x) {
  struct data* p = x ? &d : 0;
  return !(p->flag && p); // NULL_DEREF
}

int k(int x) {
  struct data* p = x ? &d : 0;
  return !(p && p->flag); // OK
}

int l(int x) {
  struct data* p = x ? &d : 0;
  return !p || p->flag; // OK
}

int l_error(int x) {
  struct data* p = x ? &d : 0;
  return p || p->flag; // NULL_DEREF
}

int m(int x) {
  struct data* p = x ? &d : 0;
  return (!p || p->flag) && !(p && p->flag);
  ; // OK
}

int n(int x) {
  struct data* p = x ? &d : 0;
  return p && (p->flag || !(p->flag)); // OK
}
