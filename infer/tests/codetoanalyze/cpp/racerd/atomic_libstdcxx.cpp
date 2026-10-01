/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <pthread.h>

// Library headers are not translated in tests, so this file mirrors how
// libstdc++ implements std::atomic<long>: its members are inherited from
// std::__atomic_base, which applies the atomic builtins to its _M_i member. Do
// not include C++ standard headers: <mutex> pulls in the real <atomic>.
namespace std {

template <typename _ITp>
struct __atomic_base {
  _ITp _M_i;

  void store(_ITp __i) { __atomic_store_n(&_M_i, __i, __ATOMIC_SEQ_CST); }
  _ITp load() const { return __atomic_load_n(&_M_i, __ATOMIC_SEQ_CST); }
  _ITp fetch_add(_ITp __i) {
    return __atomic_fetch_add(&_M_i, __i, __ATOMIC_SEQ_CST);
  }
};

template <typename _Tp>
struct atomic;

template <>
struct atomic<long> : __atomic_base<long> {};

} // namespace std

namespace atomic_libstdcxx {

class Counter {
 public:
  void store_ok(long value) {
    pthread_mutex_lock(&mutex_);
    counter_.store(value);
    plain_ = value;
    pthread_mutex_unlock(&mutex_);
  }

  long load_ok() { return counter_.load(); }

  long fetch_add_ok() { return counter_.fetch_add(1); }

  long get_plain_bad() { return plain_; }

 private:
  pthread_mutex_t mutex_;
  std::atomic<long> counter_;
  long plain_;
};

} // namespace atomic_libstdcxx
