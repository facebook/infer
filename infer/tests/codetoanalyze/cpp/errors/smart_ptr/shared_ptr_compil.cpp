/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#include <memory>

/* Compilation tests */

namespace shared_ptr_conv_from_derived {
/*
shared_ptr conversion does not work if inheritance is not public
*/
class Base {};
class Derived : public Base {};
class Q {
 protected:
  std::shared_ptr<Base> m_;

 public:
  void setM(std::shared_ptr<Base> m) { m_ = std::move(m); }
};
class P {
  std::shared_ptr<Derived> m_;
  Q q_;
  void u() { q_.setM(m_); }
};
}
