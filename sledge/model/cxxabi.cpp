/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "cxa_default_handlers.cpp"
#include "cxa_exception.cpp"
#include "cxa_exception_storage.cpp"
#include "cxa_guard.cpp"
#include "cxa_handlers.cpp"
#include "llair_intrinsics.h"

extern "C" {

void abort_message(const char* format, ...) { abort(); }

__attribute__((always_inline)) _Unwind_Reason_Code
_Unwind_RaiseException(_Unwind_Exception* unwind_exception) {
  __llair_throw(thrown_object_from_cxa_exception(
      abi::cxa_exception_from_exception_unwind_exception(unwind_exception)));
}
}
