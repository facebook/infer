/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

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
  return x * __llair_choice();
}

extern int pit;

int
root()
{
  // check that goal-directed analysis can traverse back edges when needed
  while (__llair_choice())
    pit++;
  while (__llair_choice())
    f2(__llair_choice());

  return f3(__llair_choice());
}

int
main()
{
  return root();
}
