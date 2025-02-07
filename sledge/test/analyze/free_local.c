/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma clang diagnostic ignored "-Wfree-nonheap-object"

int
main()
{
  int i;
  free(&i);
}
