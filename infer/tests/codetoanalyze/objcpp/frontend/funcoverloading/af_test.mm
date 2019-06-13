/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/NSObject.h>

int POPSelectValueType(id obj) { return 1; }

int POPSelectValueType(int v) { return v; }
