/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
void nonnull_indices(int *a, int b, int *c) __attribute__((nonnull(1, 3)));
void nonnull_all(int *a, int *b) __attribute__((nonnull));
void nonnull_param(int *a __attribute__((nonnull)));
