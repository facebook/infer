/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "header_locks.h"

// like header_locks_client.cpp, but the name of this file sorts before the
// name of the header: the deadlocks should still be reported at the first lock
namespace header_locks {

class ClientSortsBeforeHeader {
 public:
  void ab_bad() {
    a_.lock();
    b_.lock();
    b_.unlock();
    a_.unlock();
  }

  void ba_bad() {
    b_.lock();
    a_.lock();
    a_.unlock();
    b_.unlock();
  }

 private:
  Mutex a_, b_;
};

} // namespace header_locks
