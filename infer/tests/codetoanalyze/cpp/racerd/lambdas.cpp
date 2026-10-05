/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <mutex>

class Lambdas {
 public:
  void race_in_lambda_even_without_call_ok() {
    auto lambda_with_sync = [&]() {
      mutex_.lock();
      f = 0;
      mutex_.unlock();
      return f;
    };
  }

  // access propagation to callees does not currently work
  int FN_race_in_lambda_bad() {
    auto lambda_with_sync = [&]() { return g; };

    return lambda_with_sync();
  }

  void set_under_lock(int value) {
    mutex_.lock();
    g = value;
    mutex_.unlock();
  }

 private:
  int f;
  int g;
  std::mutex mutex_;
};

// unlike the call operator of a lambda, the call operator of other classes is
// reported on like any other public method
class Functor_bad {
 public:
  int operator()() { return h; }

  void set_under_lock(int value) {
    mutex_.lock();
    h = value;
    mutex_.unlock();
  }

 private:
  int h;
  std::mutex mutex_;
};

// a class is not a lambda just because its name starts with lambda_
class lambda_functor_bad {
 public:
  int operator()() { return h; }

  void set_under_lock(int value) {
    mutex_.lock();
    h = value;
    mutex_.unlock();
  }

 private:
  int h;
  std::mutex mutex_;
};

class TemplateCallOperator_bad {
 public:
  template <typename T>
  T operator()(T x) {
    return x + h;
  }

  void set_under_lock(int value) {
    mutex_.lock();
    h = value;
    mutex_.unlock();
  }

 private:
  int h;
  std::mutex mutex_;
};

int call_template_call_operator(TemplateCallOperator_bad& f) { return f(1); }

template <typename T>
class FunctorTemplate_bad {
 public:
  T operator()() { return h; }

  void set_under_lock(T value) {
    mutex_.lock();
    h = value;
    mutex_.unlock();
  }

 private:
  T h;
  std::mutex mutex_;
};

int call_functor_template(FunctorTemplate_bad<int>& f) {
  f.set_under_lock(1);
  return f();
}
