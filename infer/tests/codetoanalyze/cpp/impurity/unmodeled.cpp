/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <iostream>

void output_stream_impure() { std::cout << "Hello, world!" << std::endl; }

int random_impure() { std::rand(); }
