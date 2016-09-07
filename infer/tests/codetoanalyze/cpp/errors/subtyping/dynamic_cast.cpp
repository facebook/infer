/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

namespace dynamic__cast {

class Base {
  virtual void dummy() {}

 public:
  int b;
};
class Derived : public Base {
  int a;
};

int castOfArgumentPointer(Base* pb) {
  if (dynamic_cast<Derived*>(pb)) {
    return 1;
  } else
    return 0;
}

int castOfArgumentReference(Base& pdd) {
  Derived& pdd2 = dynamic_cast<Derived&>(pdd);
  return 0;
}

int wrongCastOfArgumentPointer() {
  Base pdd;
  return 1 / castOfArgumentPointer(&pdd);
}

int wrongCastOfArgumentReference() {
  Base pdd;
  return castOfArgumentReference(pdd);
}

int wrongPointerCast() {
  Base* pbb = new Base;
  Derived* pd = dynamic_cast<Derived*>(pbb);
  if (pd)
    return 1;
  else
    return 1 / 0;
}

int rightPointerCast() {
  Base* pbb = new Derived;
  Derived* pd = dynamic_cast<Derived*>(pbb);
  if (pd)
    return 1 / 0;
  else
    return 1;
}

void rightReferenceCast() {
  Base* pbb = new Derived;
  Derived& pdd1 = dynamic_cast<Derived&>(*pbb);
}

void wrongReferenceCast() {
  Base* pbb = new Base;
  Base& pdd = *pbb;
  Derived& pdd1 = dynamic_cast<Derived&>(pdd);
}

void wrongReferenceCastNotAssigned() {
  Base* pbb = new Base;
  Base& pdd = *pbb;
  dynamic_cast<Derived&>(pdd);
}
}
