/*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "abort_message.cpp"
#include "cxa_exception.cpp"
#include "cxa_exception_storage.cpp"
#include "cxa_handlers.cpp"
#include "llair_intrinsics.h"

extern "C" {

__attribute__((always_inline)) _Unwind_Reason_Code
_Unwind_RaiseException(_Unwind_Exception* unwind_exception) {
  __llair_throw(thrown_object_from_cxa_exception(
      abi::cxa_exception_from_exception_unwind_exception(unwind_exception)));
}
}
