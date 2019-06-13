/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

void dereference_in_array_access(int** p) {
  if (p[0])
    ;
  if ((*p)[1])
    ;
  if (p[**p])
    ;
  if ((*p)[**p])
    ;
}
