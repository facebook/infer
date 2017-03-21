/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#pragma once

#include <infer_model/infer_traits.h>
#include <infer_model/portability.h>

INFER_NAMESPACE_STD_BEGIN

int __INFER_UNLOCKED = 0;

template <class T>
void __infer_mutex_deref_first_arg(T* ptr) INFER_MODEL_AS_DEREF_FIRST_ARG;

class mutex {
 private:
  int* null_if_locked = &__INFER_UNLOCKED;

 public:
  constexpr mutex() noexcept;
  ~mutex();
  mutex(const mutex&) = delete;
  mutex& operator=(const mutex&) = delete;

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

  void unlock() {
    null_if_locked = &__INFER_UNLOCKED;
  }
};

INFER_NAMESPACE_STD_END
