/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <array>
#include <unistd.h>
#include <vector>

int pipe_array_data_ok() {
  std::array<int, 2> fds;
  if (pipe(fds.data()) == -1) {
    return -1;
  }
  close(fds[0]);
  close(fds[1]);
  return 0;
}

int pipe_vector_data_ok() {
  std::vector<int> fds(2);
  if (pipe(fds.data()) == -1) {
    return -1;
  }
  close(fds[0]);
  close(fds[1]);
  return 0;
}

// `std::array::data()` is unknown to Pulse, which does not track descriptors
// stored through the pointer it returns
int FN_pipe_array_data_not_closed_bad() {
  std::array<int, 2> fds;
  return pipe(fds.data());
}
