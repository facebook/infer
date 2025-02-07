/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

int
foo(int x)
{
  return x + __llair_choice();
}

inline int __attribute__((always_inline)) bar(int x)
{
  return x * __llair_choice();
}

// negative test case to show that inlining breaks goal-directed exploration
// when traced functions are inlined; corresponding positive test is at
// shorter_path.c; the only difference are the inlining annotations here
int
main()
{
  if (__llair_choice()) {
    foo(0);
    foo(1);
    foo(2);
    foo(3);
    foo(4);
  }
  bar(5);
};
