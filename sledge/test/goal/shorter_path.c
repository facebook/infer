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

int
bar(int x)
{
  return x * __llair_choice();
}

// small test case to ensure that we take the shorter path to the goal
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
