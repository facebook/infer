/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
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
