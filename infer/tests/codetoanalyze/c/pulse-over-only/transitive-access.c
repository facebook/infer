/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

void sink() {}

void wrapper_ok() { sink(); }

void source_bad() { wrapper_ok(); }
