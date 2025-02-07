/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <assert.h>

int
main()
{
  volatile int i = __llair_choice();

  if (0 > i || i >= 4)
    return 1;

  assert(0 <= i && i < 4);

  return 0;
}
