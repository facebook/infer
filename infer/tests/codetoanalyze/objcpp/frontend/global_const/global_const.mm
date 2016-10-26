/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

typedef struct {
  float field1;
  float field2;
  float field3;
} Fields;

static const Fields __someFields = {
    .field1 = 1, .field2 = 2, .field3 = 3,
};

Fields fields() { return __someFields; }
