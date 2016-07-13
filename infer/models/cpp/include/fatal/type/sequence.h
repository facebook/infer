/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

// TODO (t12194697) remove this file once clang in infer is updated
// Due to bug in clang, ASTExporter is unable to mangle type of
// __make_integer_seq bug report (fixed in clang's trunk):
// https://llvm.org/bugs/show_bug.cgi?id=28519
// Code that triggers the problem comes from fatal library:
// https://github.com/facebook/fatal/blob/b53547365245219f56d4c8395b8f9410da8705a3/fatal/type/sequence.h#L874
// We can't change value of FATAL_IMPL_HAS_MAKE_INTEGER_SEQ directly
// instead trick header to think that it's older clang and
// __make_integer_seq doesn't exist
#pragma push_macro("__clang_minor__")
#undef __clang_minor__
#define __clang_minor__ 7
#include_next <fatal/type/sequence.h>
#pragma pop_macro("__clang_minor__")
