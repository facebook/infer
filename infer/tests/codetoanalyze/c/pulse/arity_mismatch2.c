/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <stddef.h>

void defined_elsewhere_without_params(void) {}

int* defined_elsewhere_without_params_returns_null(void) { return NULL; }

void defined_elsewhere_with_two_params(int* p, int* q) { *q = *p; }

int* returns_null_elsewhere(void) { return NULL; }

void fails_here_returns_elsewhere(const char* msg) {}
