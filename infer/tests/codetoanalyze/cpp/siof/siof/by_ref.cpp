/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <string>

extern const std::string dangerous_object;

const std::string* init_pointer_by_ref_to_dangerous_global_good = {
    &dangerous_object};
const std::string init_pointer_by_val_to_dangerous_global_bad =
    dangerous_object;
