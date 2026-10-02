/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

int likely_unlikely_branches(int n) {
  if (n > 0) [[likely]] {
    n++;
  } else [[unlikely]] {
    n--;
  }
  return n;
}

int likely_case(int n) {
  switch (n) {
    [[likely]] case 0:
      return 1;
    [[unlikely]] default:
      return 0;
  }
}

int pragma_unroll_loop(int n) {
  int sum = 0;
#pragma unroll
  for (int i = 0; i < n; i++) {
    sum += i;
  }
  return sum;
}

int callee(int n);

int musttail_return(int n) { [[clang::musttail]] return callee(n); }

struct Destructible {
  ~Destructible();
};

int attributed_return_at_end_of_scope(int n) {
  Destructible d;
  [[likely]] return n;
}

int attributed_statements_without_nodes(int n) {
  goto out;
out:
  [[unlikely]] n;
  if (n) [[likely]]
    (void)n;
  return n;
}
