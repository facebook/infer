/*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// this file exists in gcc headers and we need to capture those includes
#include <infer_model/begin_name_override.inc>
#include_next <bits/shared_ptr.h>
#include <infer_model/end_name_override.inc>

#include <infer_model/shared_ptr.h>
