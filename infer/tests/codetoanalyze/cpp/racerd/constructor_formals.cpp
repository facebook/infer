/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <mutex>

namespace constructor_formals {

struct Y {
  Y() : rawStatus_(-3) {}
  Y(const Y& p) = default;
  Y& operator=(const Y& p) = default;
  Y(Y&& p) noexcept : rawStatus_(p.rawStatus_) { p.rawStatus_ = -3; }

  int rawStatus_;
};

struct S {
  Y w() { return returnCode; }

  Y returnCode;
};

struct SU {
  void p() {
    S s;
    auto result = s.w();
  }
};

class Basic {
 public:
  Basic() {}

  int test_locked() {
    SU su;
    mutex_.lock();
    su.p();
  }

  int test_not_locked() {
    SU su;
    su.p(); // FP fixed after adding ownership to formal parameters of
            // constructors
  }

 private:
  std::mutex mutex_;
};
} // namespace constructor_formals
