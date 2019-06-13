/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

typedef struct {
  float field1;
  float field2;
  float field3;
} Fields;

static const Fields __someFields = {
    .field1 = 1,
    .field2 = 2,
    .field3 = 3,
};

Fields fields() { return __someFields; }
