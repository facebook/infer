/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

struct Base1 {
  int b1;
};

struct Base2 {
  int b2;
};

struct Sub : public Base1, public Base2 {
  int s;
};

int div0_b1(Sub s) {
  s.b1 = 0;
  return 1 / s.b1;
}

int div0_b2(Sub s) {
  s.b2 = 0;
  return 1 / s.b2;
}

int div0_s(Sub s) {
  s.s = 0;
  return 1 / s.s;
}

int div0_cast(Sub* s) {
  s->b1 = 0;
  Base1* b = s;
  return 1 / b->b1;
}

int div0_cast_ref(Sub s) {
  s.b1 = 0;
  Base1& b = s;
  return 1 / b.b1;
}

int div0_b1_s(Sub* s) {
  s->b1 = 1;
  s->s = 1;
  return 1 / (s->b1 - s->s);
}

int div0_s_b1(Sub* s) {
  s->b1 = 1;
  s->s = 1;
  return 1 / (s->b1 - s->s);
}

int div1_b1(Sub s) {
  s.b1 = 1;
  return 1 / s.b1;
}

int div1_cast(Sub* s) {
  s->b1 = 1;
  Base1* b = s;
  return 1 / b->b1;
}
