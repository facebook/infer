/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

struct s {
  int a;
};

#ifdef __cplusplus
extern "C" {
#endif

void set_field_three(struct s* x);

#ifdef __cplusplus
}
#endif
