/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <pthread.h>

// Library headers are not translated in tests, so this file mirrors how libc++
// implements std::atomic<int>: its members are inherited from
// std::__atomic_base, which passes its __a_ member to the free functions
// std::__cxx_atomic_*, which apply the atomic builtins to a field. Do not
// include C++ standard headers: <mutex> pulls in the real <atomic>.
namespace std {
inline namespace __1 {

template <class T>
struct __cxx_atomic_base_impl {
  T __a_value;
};

template <class T>
struct __cxx_atomic_impl : public __cxx_atomic_base_impl<T> {};

template <class T>
void __cxx_atomic_store(__cxx_atomic_base_impl<T>* a, T val) {
  __atomic_store_n(&a->__a_value, val, __ATOMIC_SEQ_CST);
}

template <class T>
T __cxx_atomic_load(const __cxx_atomic_base_impl<T>* a) {
  return __atomic_load_n(&a->__a_value, __ATOMIC_SEQ_CST);
}

template <class T>
T __cxx_atomic_fetch_add(__cxx_atomic_base_impl<T>* a, T delta) {
  return __atomic_fetch_add(&a->__a_value, delta, __ATOMIC_SEQ_CST);
}

template <class T>
struct __atomic_base {
  mutable __cxx_atomic_impl<T> __a_;

  void store(T d) { __cxx_atomic_store(&__a_, d); }
  T load() const { return __cxx_atomic_load(&__a_); }
  T fetch_add(T op) { return __cxx_atomic_fetch_add(&__a_, op); }
  T operator++(int) { return fetch_add(T(1)); }
};

template <class T>
struct atomic : public __atomic_base<T> {};

} // namespace __1
} // namespace std

namespace atomic_libcxx {

class Counter {
 public:
  void store_ok(int value) {
    pthread_mutex_lock(&mutex_);
    counter_.store(value);
    plain_ = value;
    pthread_mutex_unlock(&mutex_);
  }

  int load_ok() { return counter_.load(); }

  int increment_ok() { return counter_++; }

  int get_plain_bad() { return plain_; }

 private:
  pthread_mutex_t mutex_;
  std::atomic<int> counter_;
  int plain_;
};

} // namespace atomic_libcxx
