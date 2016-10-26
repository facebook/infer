/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#include <stdexcept>

namespace cast_with_enforce {

class WrongParameterException : public std::runtime_error {
 public:
  WrongParameterException(bool e, const char* msg) : std::runtime_error(msg){};
};

#define ENFORCE(e, ...) \
  ENFORCE_CUSTOM(WrongParameterException, e, ##__VA_ARGS__)

#define ENFORCE_CUSTOM(exception, e, ...)           \
  ({                                                \
    auto const& _tmp = (e);                         \
    _tmp ? _tmp : throw exception(#e, __VA_ARGS__); \
  })

class Base {
  virtual void dummy() {}

 public:
  int b;
};
class Derived : public Base {
 public:
  int a;
};

int cast_with_no_npe(Base& certificate) {
  auto cert = dynamic_cast<Derived*>(&certificate);
  ENFORCE(cert, "Base is not Derived");
  return cert->a;
}

int cast_with_npe_avoided_by_enforce() {
  Base base;
  auto derived = dynamic_cast<Derived*>(&base);
  ENFORCE(derived, "Base is not Derived");
  return derived->a; // shouldn't result in NPE, even though it really is a
  // problem
}

int cast_with_npe() {
  Base base;
  auto derived = dynamic_cast<Derived*>(&base);
  return derived->a; // npe
}
}
