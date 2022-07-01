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

extern int pit;

int
main()
{
  int a = foo(5);
  int b = bar(b);
}
