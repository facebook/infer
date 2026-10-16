/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <mutex>

void callback1();
void callback2();

// writes of function pointers are not races
class FunctionPointer {
 public:
  void set_either(bool b) {
    void (*callback)() = &callback1;
    if (b) {
      callback = &callback2;
    }
    std::lock_guard<std::mutex> lock(mutex_);
    callback_ = callback;
  }

  void set_or_null(bool b) {
    void (*callback)() = nullptr;
    if (b) {
      callback = &callback1;
    }
    std::lock_guard<std::mutex> lock(mutex_);
    other_callback_ = callback;
  }

  void call_ok() {
    if (callback_) {
      callback_();
    }
  }

  void call_other_ok() {
    if (other_callback_) {
      other_callback_();
    }
  }

 private:
  std::mutex mutex_;
  void (*callback_)() = nullptr;
  void (*other_callback_)() = nullptr;
};
