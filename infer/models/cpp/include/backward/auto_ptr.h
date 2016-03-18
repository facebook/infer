/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

// this file exists in gcc headers and we need to capture those includes
#include <infer_model/begin_name_override.inc>
#include_next <backward/auto_ptr.h>
#include <infer_model/end_name_override.inc>
