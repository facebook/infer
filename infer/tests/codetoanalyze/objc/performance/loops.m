/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
// j is not a control var, so shouldn't affect the bound
int if_in_loop(int t) {
  int p = 0;
  int j = t + 1;
  for (int i = 0; i < 5; i++) {
    if (j < 2) {
      p++;
    } else {
      p = 3;
      for (int k = 0; k < 10; k++) {
        int m = 0;
      }
    }
  }
  return p;
}

// j is not a control var, so shouldn't affect the bound
int if_out_loop(int t) {
  int p = 10;
  int j = t + 10;
  if (j < 2) {
    p++;
  } else {
    p = 3;
    for (int k = 0; k < 100; k++) {
      int m = 0;
    }
  }
  return p;
}

int do_while_independent_of_p(int p) {
  int a = 0;
  do {
    if (p == 15) {
      p = p + 1;
    }
    a++;
  } while (a < 25);

  return 0;
}

void larger_state_FN() {

  int i = 0, k = 0;
  while (k < 100) {
    i++;
    if (i >= 10000) {
      k++;
      i = 0;
    }
  }
}

static int array1[] = {1, 2, 3};
static int array2[] = {};

// Cvars will initially contain array1 and array2 but will be removed
// since they are invariant
void loop_use_global_vars(int x) {
  for (int i = 0; i < x && array1 != array2; i++) {
    // do something
  }
}

void ptr_cmp(char* end, int size) {
  char buf[2] = "hi";
  for (int i = 0; i < size; i += 2) {
    if (buf < end) { // pvar &buf occurs directly in prune node
      return;
    }
  }
}
