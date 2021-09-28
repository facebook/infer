/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

int count = 0;

void child_routine() { count++; }

int main() {
  thread_t* child;
  error_t err = thread_create(&child, &child_routine);
  count++;
  err = thread_join(child);
  return count;
}
