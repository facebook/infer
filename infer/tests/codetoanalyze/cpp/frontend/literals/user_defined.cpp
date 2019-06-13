/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

int operator"" _literal(unsigned long long i) { return i; }

int foo() { return 0_literal; }
