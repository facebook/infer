/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "crossfile-1.h"

// a deadlock should be reported here
void CrossFileOne::lock_my_mutex_first_then_the_other() {
  _mutex.lock();
  _other->just_lock_my_mutex();
  _mutex.unlock();
}

void CrossFileOne::just_lock_my_mutex() {
  _mutex.lock();
  _mutex.unlock();
}
