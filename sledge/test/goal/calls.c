/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// small test case intended to exercise sparse-trace goal-directed execution

int
f0(int x)
{
  return x + __llair_choice();
}

int
f1(int x)
{
  return f0(x + x);
}

int
f2(int x)
{
  return f1(x + x);
}

int
f3(int x)
{
  return f2(x + x);
}

int
foo(int x)
{
  return x * __llair_choice();
}

int
g3(int x)
{
  return foo(x);
}

int
g2(int x)
{
  return g3(x);
}

int
g1(int x)
{
  return g2(x);
}

int
g0(int x)
{
  return g1(x);
}

int
bar()
{
  return g0(0);
}

int
baz()
{
  return foo(0);
}

int
main()
{
  // mimics a taint analysis trace from source f0 to sink g3
  int y = f3(1);
  int z = g0(y);

  // for testing goal-directed distances:
  // false branch reaches foo more quickly than true branch
  if (z) {
    bar();
  } else {
    baz();
  }
};
