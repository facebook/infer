/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

int same_tu_foo();
int same_tu_goo();

// BAD: report SIOF here
// This may not get the initialized value for y.
// Infer doesn't yet report here because it only looks across translation units.
int same_tu_x = same_tu_foo();
int same_tu_y = same_tu_goo();

int same_tu_foo() { return same_tu_y; }

int same_tu_goo() { return 42; }
