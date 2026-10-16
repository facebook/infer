/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

struct Guard {
  ~Guard() {}
};

constexpr int twice(int x) { return 2 * x; }

template <int N>
int if_constexpr_template() {
  if constexpr (twice(N) == 0) {
    return 0;
  } else if constexpr (twice(N) == 6) {
    return N;
  } else {
    return -1;
  }
}

int call_if_constexpr_template() { return if_constexpr_template<3>(); }

int if_constexpr_non_template(int x) {
  if constexpr (sizeof(int) == 4) {
    return x;
  } else {
    return -x;
  }
}

int if_constexpr_false_no_else(int x) {
  if constexpr (sizeof(int) == 1) {
    x = 0;
  }
  return x;
}

int if_constexpr_single_statement_branches(int x) {
  if constexpr (sizeof(int) == 1)
    x = 1;
  else if constexpr (sizeof(int) == 4)
    x = 4;
  else
    x = 0;
  return x;
}

int if_constexpr_init_statement(int x) {
  if constexpr (Guard g; sizeof(int) > 1) {
    x++;
  }
  return x;
}

int if_constexpr_condition_variable(int x) {
  if constexpr (constexpr int n = sizeof(int)) {
    x += n;
  }
  return x;
}

int case_label_constant(int x) {
  switch (x) {
    case twice(2):
      return 1;
    default:
      return 0;
  }
}
