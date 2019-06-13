/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
extern const int const_x;
extern const int const_y;
extern int z;
extern int u;

int use_x = const_x + 1;
int use_y = const_y + 1;
int use_z = z + 1;
int use_u = u + 1;
