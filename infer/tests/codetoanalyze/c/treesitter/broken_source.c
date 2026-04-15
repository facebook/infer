/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

/* This file has intentional syntax errors to demonstrate that the tree-sitter
   frontend can still find bugs in code that does not compile.
   The clang frontend would reject this file entirely. */

/* Missing semicolon after struct — tree-sitter parses around the error */
struct Broken {
  int x
  int y;
}

/* Valid function with a real bug — tree-sitter finds this despite
   the broken struct above */
void null_deref_in_broken_file_Bad() {
  int* p = 0;
  *p = 42;
}

/* Another function with a missing closing brace for an if —
   tree-sitter still parses the null deref */
void another_bug_Bad(int* p) {
  if (p == 0) {
    int* q = 0;
    *q = 1;
}
