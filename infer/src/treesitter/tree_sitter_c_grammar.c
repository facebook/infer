/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

/* tree-sitter C grammar parser tables.
 * Compiled as a separate translation unit from the runtime.
 * Use a path that bypasses the runtime's -I to avoid picking up
 * the wrong parser.c (both directories have one). */
#include "tree-sitter-c-parser.c"
