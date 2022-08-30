/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

int foo(int x);
int bar(int x);
int baz(int x);

int
baz(int x)
{
  return x + __llair_choice();
}

// recursion over a direct call
int
bar(int x)
{
  return __llair_choice() ? bar(x) : baz(x);
}

// recursion over an indirect call
int
foo(int x)
{
  int (*f)(int) = &foo;
  if (__llair_choice()) {
    f = &bar;
  }

  return x + f(x);
}

int
root()
{
  // check that distance computation doesn't get stuck in recursive cycles
  return foo(5);
}

int
main()
{
  return root();
}
