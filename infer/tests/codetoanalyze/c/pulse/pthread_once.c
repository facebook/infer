/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <pthread.h>

// Side-effecting callback: sets the static `slot` to NULL so a deref
// after `pthread_once` returns is a real NPE — but only visible if
// Pulse actually traverses into the init callback.
static int* slot;

void init_to_null() { slot = (int*)0; }

int pthread_once_deref_bad() {
  // Each call gets its own once_control to keep tests independent.
  pthread_once_t once = PTHREAD_ONCE_INIT;
  // Start from a non-null value so the bug is reachable only if the
  // callback actually executes inside Pulse's model.
  int x = 0;
  slot = &x;
  pthread_once(&once, init_to_null);
  return *slot; // NPE: init_to_null set slot to NULL via the callback
}

void init_to_nonnull() {
  static int y = 7;
  slot = &y;
}

int pthread_once_deref_ok() {
  pthread_once_t once = PTHREAD_ONCE_INIT;
  slot = (int*)0;
  pthread_once(&once, init_to_nonnull);
  return *slot; // no NPE: init_to_nonnull pointed slot at a valid storage
}
