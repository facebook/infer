/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <functional>
#include <ios>
#include <new>
#include <stdexcept>
#include <system_error>
#include <typeinfo>

namespace std {

void __throw_bad_alloc() { throw bad_alloc(); }
void __throw_bad_cast() { throw bad_cast(); }
void __throw_bad_exception() { throw bad_exception(); }
void __throw_bad_function_call() { throw bad_function_call(); }
void __throw_bad_typeid() { throw bad_typeid(); }
void __throw_domain_error(const char* s) { throw domain_error(s); }
void __throw_invalid_argument(const char* s) { throw invalid_argument(s); }
void __throw_ios_failure(const char* s) { throw ios_base::failure(s); }
void __throw_length_error(const char* s) { throw length_error(s); }
void __throw_logic_error(const char* s) { throw logic_error(s); }
void __throw_out_of_range(const char* s) { throw out_of_range(s); }
void __throw_overflow_error(const char* s) { throw overflow_error(s); }
void __throw_range_error(const char* s) { throw range_error(s); }
void __throw_runtime_error(const char* s) { throw runtime_error(s); }
void __throw_system_error(int c) {
  throw system_error(error_code(c, generic_category()));
}
void __throw_underflow_error(const char* s) { throw underflow_error(s); }
} // namespace std
