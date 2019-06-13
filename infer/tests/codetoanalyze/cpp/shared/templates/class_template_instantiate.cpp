/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

struct Choose1 {
  int div(int a, int b) { return 1 / a; }
};

struct Choose2 {
  int div(int a, int b) { return 1 / b; }

  // extra method that is not present in Choose1
  int extra(int a) { return 1 / a; }
};

template <class T>
struct ExecStore {
  T f;
  int call_div(int a) {
    // this will always cause div by 0 for Choose2
    return f.div(a, 0);
  }
};

int choose1_div0(ExecStore<Choose1>& s) { return s.call_div(0); }

int choose1_div1(ExecStore<Choose1>& s) { return s.call_div(1); }

int choose2_div0_no_report(ExecStore<Choose2>& s) {
  // error is already reported for ExecStore<Choose2>::call_div()
  return s.call_div(1);
}

int choose2_div0_extra(ExecStore<Choose2>& s) { return s.f.extra(0); }

int choose2_div1_extra(ExecStore<Choose2>& s) { return s.f.extra(1); }
