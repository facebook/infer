/*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

#include <chrono>
#include <infer_model/infer_traits.h>
#include <infer_model/portability.h>

INFER_NAMESPACE_STD_BEGIN

int __INFER_UNLOCKED = 0;

template <class T>
void __infer_mutex_deref_first_arg(T* ptr) INFER_MODEL_AS_DEREF_FIRST_ARG;

class __infer_mutex_model {
 private:
  int* null_if_locked = &__INFER_UNLOCKED;

 public:
  constexpr __infer_mutex_model() noexcept;
  ~__infer_mutex_model();
  __infer_mutex_model(const __infer_mutex_model&) = delete;
  __infer_mutex_model& operator=(const __infer_mutex_model&) = delete;

  void lock() {
    __infer_mutex_deref_first_arg(null_if_locked); // HACK
    null_if_locked = nullptr;
  }

  bool try_lock() {
    if (null_if_locked) {
      null_if_locked = nullptr;
      return true;
    } else {
      return false;
    }
  }

  void unlock() { null_if_locked = &__INFER_UNLOCKED; }
};

class mutex {
 private:
  __infer_mutex_model mutex_model;

 public:
  void lock() { mutex_model.lock(); }

  bool try_lock() { return mutex_model.try_lock(); }

  void unlock() { mutex_model.unlock(); }
};

class timed_mutex {
 private:
  __infer_mutex_model mutex_model;

 public:
  void lock() { mutex_model.lock(); }

  bool try_lock() { return mutex_model.try_lock(); }

  void unlock() { mutex_model.unlock(); }

  template <class Rep, class Period>
  bool try_lock_for(const chrono::duration<Rep, Period>& rel_time) {
    return mutex_model.try_lock();
  }

  template <class Clock, class Duration>
  bool try_lock_until(const chrono::time_point<Clock, Duration>& abs_time) {
    return mutex_model.try_lock();
  }
};

INFER_NAMESPACE_STD_END
