/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

#include <infer_model/common.h>
#include <infer_model/infer_traits.h>

#include <cstdint>
#include <cstddef>
#include <cstring>

INFER_NAMESPACE_STD_BEGIN

typedef enum memory_order {
  memory_order_relaxed,
  memory_order_consume,
  memory_order_acquire,
  memory_order_release,
  memory_order_acq_rel,
  memory_order_seq_cst
} memory_order;

template <typename T>
T kill_dependency(T y) noexcept {
  return y;
}

// lock-free property
#define ATOMIC_BOOL_LOCK_FREE 2
#define ATOMIC_CHAR_LOCK_FREE 2
#define ATOMIC_CHAR16_T_LOCK_FREE 2
#define ATOMIC_CHAR32_T_LOCK_FREE 2
#define ATOMIC_WCHAR_T_LOCK_FREE 2
#define ATOMIC_SHORT_LOCK_FREE 2
#define ATOMIC_INT_LOCK_FREE 2
#define ATOMIC_LONG_LOCK_FREE 2
#define ATOMIC_LLONG_LOCK_FREE 2
#define ATOMIC_POINTER_LOCK_FREE 2

// NOTE1: We are missing the atomic<bool> specialization here. Will need to add
// it when necessary
// NOTE2: In reality, most of the atomic operations, including load, store,
// exchange, etc. are implemented with compiler intrinsics. Some of them are
// really hard to emulate with a source-level model due to various constraints
// in the source language. We try our best here to violate as few constraints as
// possible, but if it turns out to be a problem in practice, we may need to
// consider moving some of the model implementation to BuiltinDefn.

template <typename T>
struct __infer_atomic_base {
  bool is_lock_free() const volatile noexcept { return false; }
  bool is_lock_free() const noexcept { return false; }

  // Note that the biabduction analysis has a hard time understanding memcpy
  // with non-array args. If this function turns out to be a blocker for the
  // backend, consider moving it to BuiltinDefn.
  void store(T t, memory_order mo = memory_order_seq_cst) volatile noexcept {
    memcpy(&_wrapped_value, &t, sizeof(T));
  }
  void store(T t, memory_order mo = memory_order_seq_cst) noexcept {
    memcpy(&_wrapped_value, &t, sizeof(T));
  }

  T load(memory_order mo = memory_order_seq_cst) const volatile noexcept {
    return _wrapped_value;
  }
  T load(memory_order mo = memory_order_seq_cst) const noexcept {
    return _wrapped_value;
  }
  operator T() const volatile noexcept { return _wrapped_value; }
  operator T() const noexcept { return _wrapped_value; }

  // Note that these two functions are not standard-compliant: if T is not
  // copy-constructible, then we are going to get a compilation failure.
  // The body of this function could be a candidate for BuiltinDefn, if
  // necessary.
  T exchange(T t, memory_order mo = memory_order_seq_cst) volatile noexcept {
    T tmp = _wrapped_value;
    _wrapped_value = t;
    return tmp;
  }
  T exchange(T t, memory_order mo = memory_order_seq_cst) noexcept {
    T tmp = _wrapped_value;
    _wrapped_value = t;
    return tmp;
  }

  // For arbitrary type T, we have to rely on memcmp/memcpy instead of
  // operator==/operator= for comparison/assignment
  // Note that the biabduction analysis has a hard time understanding memcpy
  // with non-array args. If this function turns out to be a blocker for the
  // backend, consider moving it to BuiltinDefn.
  bool __infer_compare_exchange_impl(T* expected,
                                     T* desired) volatile noexcept {
    if (memcmp(&_wrapped_value, expected, sizeof(T))) {
      memcpy(&_wrapped_value, desired, sizeof(T));
      return true;
    } else {
      memcpy(expected, &_wrapped_value, sizeof(T));
      return false;
    }
  }
  bool __infer_compare_exchange_impl(T* expected, T* desired) noexcept {
    if (memcmp(&_wrapped_value, expected, sizeof(T))) {
      memcpy(&_wrapped_value, desired, sizeof(T));
      return true;
    } else {
      memcpy(expected, &_wrapped_value, sizeof(T));
      return false;
    }
  }

  bool compare_exchange_weak(
      T& expected,
      T desired,
      memory_order mo = memory_order_seq_cst) volatile noexcept {
    return __infer_compare_exchange_impl(&expected, &desired);
  }
  bool compare_exchange_weak(T& expected,
                             T desired,
                             memory_order mo = memory_order_seq_cst) noexcept {
    return __infer_compare_exchange_impl(&expected, &desired);
  }
  bool compare_exchange_weak(T& expected,
                             T desired,
                             memory_order succ,
                             memory_order fail) volatile noexcept {
    return __infer_compare_exchange_impl(&expected, &desired);
  }
  bool compare_exchange_weak(T& expected,
                             T desired,
                             memory_order succ,
                             memory_order fail) noexcept {
    return __infer_compare_exchange_impl(&expected, &desired);
  }
  bool compare_exchange_strong(
      T& expected,
      T desired,
      memory_order mo = memory_order_seq_cst) volatile noexcept {
    return __infer_compare_exchange_impl(&expected, &desired);
  }
  bool compare_exchange_strong(
      T& expected, T desired, memory_order mo = memory_order_seq_cst) noexcept {
    return __infer_compare_exchange_impl(&expected, &desired);
  }
  bool compare_exchange_strong(T& expected,
                               T desired,
                               memory_order succ,
                               memory_order fail) volatile noexcept {
    return __infer_compare_exchange_impl(&expected, &desired);
  }
  bool compare_exchange_strong(T& expected,
                               T desired,
                               memory_order succ,
                               memory_order fail) noexcept {
    return __infer_compare_exchange_impl(&expected, &desired);
  }

  __infer_atomic_base() noexcept = default;
  constexpr __infer_atomic_base(T desired) : _wrapped_value(desired) {}
  __infer_atomic_base(const __infer_atomic_base&) = delete;
  __infer_atomic_base& operator=(const __infer_atomic_base&) = delete;
  __infer_atomic_base& operator=(const __infer_atomic_base&) volatile = delete;
  T operator=(T other) volatile noexcept {
    store(other);
    return other;
  }
  T operator=(T other) noexcept {
    store(other);
    return other;
  }

  T _wrapped_value;
};

template <typename T>
struct __infer_atomic_integral : public __infer_atomic_base<T> {
  typedef __infer_atomic_base<T> __base;
  __infer_atomic_integral() noexcept = default;
  constexpr __infer_atomic_integral(T d) noexcept : __base(d) {}

  bool is_lock_free() const volatile noexcept { return true; }
  bool is_lock_free() const noexcept { return true; }
  void store(T t, memory_order mo = memory_order_seq_cst) volatile noexcept {
    this->_wrapped_value = t;
  }
  void store(T t, memory_order mo = memory_order_seq_cst) noexcept {
    this->_wrapped_value = t;
  }
  T load(memory_order mo = memory_order_seq_cst) const volatile noexcept {
    return this->_wrapped_value;
  }
  T load(memory_order mo = memory_order_seq_cst) const noexcept {
    return this->_wrapped_value;
  }
  operator T() const volatile noexcept { return this->_wrapped_value; }
  operator T() const noexcept { return this->_wrapped_value; }

  bool __infer_compare_exchange_impl(T& expected, T desired) volatile noexcept {
    if (this->_wrapped_value == expected) {
      this->_wrapped_value = desired;
      return true;
    } else {
      expected = this->_wrapped_value;
      return false;
    }
  }
  bool __infer_compare_exchange_impl(T& expected, T desired) noexcept {
    if (this->_wrapped_value == expected) {
      this->_wrapped_value = desired;
      return true;
    } else {
      expected = this->_wrapped_value;
      return false;
    }
  }

  bool compare_exchange_weak(
      T& expected,
      T desired,
      memory_order mo = memory_order_seq_cst) volatile noexcept {
    return __infer_compare_exchange_impl(expected, desired);
  }
  bool compare_exchange_weak(T& expected,
                             T desired,
                             memory_order mo = memory_order_seq_cst) noexcept {
    return __infer_compare_exchange_impl(expected, desired);
  }
  bool compare_exchange_weak(T& expected,
                             T desired,
                             memory_order succ,
                             memory_order fail) volatile noexcept {
    return __infer_compare_exchange_impl(expected, desired);
  }
  bool compare_exchange_weak(T& expected,
                             T desired,
                             memory_order succ,
                             memory_order fail) noexcept {
    return __infer_compare_exchange_impl(expected, desired);
  }
  bool compare_exchange_strong(
      T& expected,
      T desired,
      memory_order mo = memory_order_seq_cst) volatile noexcept {
    return __infer_compare_exchange_impl(expected, desired);
  }
  bool compare_exchange_strong(
      T& expected, T desired, memory_order mo = memory_order_seq_cst) noexcept {
    return __infer_compare_exchange_impl(expected, desired);
  }
  bool compare_exchange_strong(T& expected,
                               T desired,
                               memory_order succ,
                               memory_order fail) volatile noexcept {
    return __infer_compare_exchange_impl(expected, desired);
  }
  bool compare_exchange_strong(T& expected,
                               T desired,
                               memory_order succ,
                               memory_order fail) noexcept {
    return __infer_compare_exchange_impl(expected, desired);
  }

  T operator=(T other) volatile noexcept {
    this->_wrapped_value = other;
    return other;
  }
  T operator=(T other) noexcept {
    this->_wrapped_value = other;
    return other;
  }

  T fetch_add(T op, memory_order mo = memory_order_seq_cst) volatile noexcept {
    T ret = this->_wrapped_value;
    this->_wrapped_value += op;
    return ret;
  }
  T fetch_add(T op, memory_order mo = memory_order_seq_cst) noexcept {
    T ret = this->_wrapped_value;
    this->_wrapped_value += op;
    return ret;
  }
  T fetch_sub(T op, memory_order mo = memory_order_seq_cst) volatile noexcept {
    T ret = this->_wrapped_value;
    this->_wrapped_value -= op;
    return ret;
  }
  T fetch_sub(T op, memory_order mo = memory_order_seq_cst) noexcept {
    T ret = this->_wrapped_value;
    this->_wrapped_value -= op;
    return ret;
  }
  T fetch_and(T op, memory_order mo = memory_order_seq_cst) volatile noexcept {
    T ret = this->_wrapped_value;
    this->_wrapped_value &= op;
    return ret;
  }
  T fetch_and(T op, memory_order mo = memory_order_seq_cst) noexcept {
    T ret = this->_wrapped_value;
    this->_wrapped_value &= op;
    return ret;
  }
  T fetch_or(T op, memory_order mo = memory_order_seq_cst) volatile noexcept {
    T ret = this->_wrapped_value;
    this->_wrapped_value |= op;
    return ret;
  }
  T fetch_or(T op, memory_order mo = memory_order_seq_cst) noexcept {
    T ret = this->_wrapped_value;
    this->_wrapped_value |= op;
    return ret;
  }
  T fetch_xor(T op, memory_order mo = memory_order_seq_cst) volatile noexcept {
    T ret = this->_wrapped_value;
    this->_wrapped_value ^= op;
    return ret;
  }
  T fetch_xor(T op, memory_order mo = memory_order_seq_cst) noexcept {
    T ret = this->_wrapped_value;
    this->_wrapped_value ^= op;
    return ret;
  }

  T operator++(int) volatile noexcept { return fetch_add(T(1)); }
  T operator++(int) noexcept { return fetch_add(T(1)); }
  T operator--(int) volatile noexcept { return fetch_sub(T(1)); }
  T operator--(int) noexcept { return fetch_sub(T(1)); }
  T operator++() volatile noexcept { return fetch_add(T(1)) + T(1); }
  T operator++() noexcept { return fetch_add(T(1)) + T(1); }
  T operator--() volatile noexcept { return fetch_sub(T(1)) - T(1); }
  T operator--() noexcept { return fetch_sub(T(1)) - T(1); }
  T operator+=(T op) volatile noexcept { return fetch_add(op) + op; }
  T operator+=(T op) noexcept { return fetch_add(op) + op; }
  T operator-=(T op) volatile noexcept { return fetch_sub(op) - op; }
  T operator-=(T op) noexcept { return fetch_sub(op) - op; }
  T operator&=(T op) volatile noexcept { return fetch_and(op) & op; }
  T operator&=(T op) noexcept { return fetch_and(op) & op; }
  T operator|=(T op) volatile noexcept { return fetch_or(op) | op; }
  T operator|=(T op) noexcept { return fetch_or(op) | op; }
  T operator^=(T op) volatile noexcept { return fetch_xor(op) ^ op; }
  T operator^=(T op) noexcept { return fetch_xor(op) ^ op; }
};

template <typename T>
struct atomic : public __infer_atomic_base<T> {
  typedef __infer_atomic_base<T> __base;
  atomic() noexcept = default;
  constexpr atomic(T d) noexcept : __base(d) {}

  T operator=(T d) volatile noexcept {
    __base::store(d);
    return d;
  }
  T operator=(T d) noexcept {
    __base::store(d);
    return d;
  }
};

template <>
struct atomic<char> : public __infer_atomic_integral<char> {
  typedef __infer_atomic_integral<char> __base;
  typedef char __integral_type;
  atomic() noexcept = default;
  constexpr atomic(__integral_type d) noexcept : __base(d) {}

  using __base::operator=;
};
template <>
struct atomic<signed char> : public __infer_atomic_integral<signed char> {
  typedef __infer_atomic_integral<signed char> __base;
  typedef signed char __integral_type;
  atomic() noexcept = default;
  constexpr atomic(__integral_type d) noexcept : __base(d) {}

  using __base::operator=;
};
template <>
struct atomic<unsigned char> : public __infer_atomic_integral<unsigned char> {
  typedef __infer_atomic_integral<unsigned char> __base;
  typedef unsigned char __integral_type;
  atomic() noexcept = default;
  constexpr atomic(__integral_type d) noexcept : __base(d) {}

  using __base::operator=;
};
template <>
struct atomic<short> : public __infer_atomic_integral<short> {
  typedef __infer_atomic_integral<short> __base;
  typedef short __integral_type;
  atomic() noexcept = default;
  constexpr atomic(__integral_type d) noexcept : __base(d) {}

  using __base::operator=;
};
template <>
struct atomic<unsigned short> : public __infer_atomic_integral<unsigned short> {
  typedef __infer_atomic_integral<unsigned short> __base;
  typedef unsigned short __integral_type;
  atomic() noexcept = default;
  constexpr atomic(__integral_type d) noexcept : __base(d) {}

  using __base::operator=;
};
template <>
struct atomic<int> : public __infer_atomic_integral<int> {
  typedef __infer_atomic_integral<int> __base;
  typedef int __integral_type;
  atomic() noexcept = default;
  constexpr atomic(__integral_type d) noexcept : __base(d) {}

  using __base::operator=;
};
template <>
struct atomic<unsigned int> : public __infer_atomic_integral<unsigned int> {
  typedef __infer_atomic_integral<unsigned int> __base;
  typedef unsigned int __integral_type;
  atomic() noexcept = default;
  constexpr atomic(__integral_type d) noexcept : __base(d) {}

  using __base::operator=;
};
template <>
struct atomic<long> : public __infer_atomic_integral<long> {
  typedef __infer_atomic_integral<long> __base;
  typedef long __integral_type;
  atomic() noexcept = default;
  constexpr atomic(__integral_type d) noexcept : __base(d) {}

  using __base::operator=;
};
template <>
struct atomic<unsigned long> : public __infer_atomic_integral<unsigned long> {
  typedef __infer_atomic_integral<unsigned long> __base;
  typedef unsigned long __integral_type;
  atomic() noexcept = default;
  constexpr atomic(__integral_type d) noexcept : __base(d) {}

  using __base::operator=;
};
template <>
struct atomic<long long> : public __infer_atomic_integral<long long> {
  typedef __infer_atomic_integral<long long> __base;
  typedef long long __integral_type;
  atomic() noexcept = default;
  constexpr atomic(__integral_type d) noexcept : __base(d) {}

  using __base::operator=;
};
template <>
struct atomic<unsigned long long>
    : public __infer_atomic_integral<unsigned long long> {
  typedef __infer_atomic_integral<unsigned long long> __base;
  typedef unsigned long long __integral_type;
  atomic() noexcept = default;
  constexpr atomic(__integral_type d) noexcept : __base(d) {}

  using __base::operator=;
};
template <>
struct atomic<wchar_t> : public __infer_atomic_integral<wchar_t> {
  typedef __infer_atomic_integral<wchar_t> __base;
  typedef wchar_t __integral_type;
  atomic() noexcept = default;
  constexpr atomic(__integral_type d) noexcept : __base(d) {}

  using __base::operator=;
};
template <>
struct atomic<char16_t> : public __infer_atomic_integral<char16_t> {
  typedef __infer_atomic_integral<char16_t> __base;
  typedef char16_t __integral_type;
  atomic() noexcept = default;
  constexpr atomic(__integral_type d) noexcept : __base(d) {}

  using __base::operator=;
};
template <>
struct atomic<char32_t> : public __infer_atomic_integral<char32_t> {
  typedef __infer_atomic_integral<char32_t> __base;
  typedef char32_t __integral_type;
  atomic() noexcept = default;
  constexpr atomic(__integral_type d) noexcept : __base(d) {}

  using __base::operator=;
};

template <typename T>
struct atomic<T*> : public __infer_atomic_base<T*> {
  typedef __infer_atomic_base<T*> __base;
  atomic() noexcept = default;
  constexpr atomic(T* d) noexcept : __base(d) {}

  bool is_lock_free() const volatile noexcept { return true; }
  bool is_lock_free() const noexcept { return true; }
  void store(T* t, memory_order mo = memory_order_seq_cst) volatile noexcept {
    this->_wrapped_value = t;
  }
  void store(T* t, memory_order mo = memory_order_seq_cst) noexcept {
    this->_wrapped_value = t;
  }
  T* load(memory_order mo = memory_order_seq_cst) const volatile noexcept {
    return this->_wrapped_value;
  }
  T* load(memory_order mo = memory_order_seq_cst) const noexcept {
    return this->_wrapped_value;
  }
  operator T*() const volatile noexcept { return this->_wrapped_value; }
  operator T*() const noexcept { return this->_wrapped_value; }

  bool __infer_compare_exchange_impl(T*& expected,
                                     T* desired) volatile noexcept {
    if (this->_wrapped_value == expected) {
      this->_wrapped_value = desired;
      return true;
    } else {
      expected = this->_wrapped_value;
      return false;
    }
  }
  bool __infer_compare_exchange_impl(T*& expected, T* desired) noexcept {
    if (this->_wrapped_value == expected) {
      this->_wrapped_value = desired;
      return true;
    } else {
      expected = this->_wrapped_value;
      return false;
    }
  }
  bool compare_exchange_weak(
      T*& expected,
      T* desired,
      memory_order mo = memory_order_seq_cst) volatile noexcept {
    return __infer_compare_exchange_impl(expected, desired);
  }
  bool compare_exchange_weak(T*& expected,
                             T* desired,
                             memory_order mo = memory_order_seq_cst) noexcept {
    return __infer_compare_exchange_impl(expected, desired);
  }
  bool compare_exchange_weak(T*& expected,
                             T* desired,
                             memory_order succ,
                             memory_order fail) volatile noexcept {
    return __infer_compare_exchange_impl(expected, desired);
  }
  bool compare_exchange_weak(T*& expected,
                             T* desired,
                             memory_order succ,
                             memory_order fail) noexcept {
    return __infer_compare_exchange_impl(expected, desired);
  }
  bool compare_exchange_strong(
      T*& expected,
      T* desired,
      memory_order mo = memory_order_seq_cst) volatile noexcept {
    return __infer_compare_exchange_impl(expected, desired);
  }
  bool compare_exchange_strong(
      T*& expected,
      T* desired,
      memory_order mo = memory_order_seq_cst) noexcept {
    return __infer_compare_exchange_impl(expected, desired);
  }
  bool compare_exchange_strong(T*& expected,
                               T* desired,
                               memory_order succ,
                               memory_order fail) volatile noexcept {
    return __infer_compare_exchange_impl(expected, desired);
  }
  bool compare_exchange_strong(T*& expected,
                               T* desired,
                               memory_order succ,
                               memory_order fail) noexcept {
    return __infer_compare_exchange_impl(expected, desired);
  }

  T* operator=(T* d) volatile noexcept {
    this->_wrapped_value = d;
    return d;
  }
  T* operator=(T* d) noexcept {
    this->_wrapped_value = d;
    return d;
  }

  T* fetch_add(ptrdiff_t op,
               memory_order mo = memory_order_seq_cst) volatile noexcept {
    T* ret = this->_wrapped_value;
    this->_wrapped_value += op;
    return ret;
  }
  T* fetch_add(ptrdiff_t op, memory_order mo = memory_order_seq_cst) noexcept {
    T* ret = this->_wrapped_value;
    this->_wrapped_value += op;
    return ret;
  }
  T* fetch_sub(ptrdiff_t op,
               memory_order mo = memory_order_seq_cst) volatile noexcept {
    T* ret = this->_wrapped_value;
    this->_wrapped_value -= op;
    return ret;
  }
  T* fetch_sub(ptrdiff_t op, memory_order mo = memory_order_seq_cst) noexcept {
    T* ret = this->_wrapped_value;
    this->_wrapped_value -= op;
    return ret;
  }

  T* operator++(int) volatile noexcept { return fetch_add(1); }
  T* operator++(int) noexcept { return fetch_add(1); }
  T* operator--(int) volatile noexcept { return fetch_sub(1); }
  T* operator--(int) noexcept { return fetch_sub(1); }
  T* operator++() volatile noexcept { return fetch_add(1) + 1; }
  T* operator++() noexcept { return fetch_add(1) + T(1); }
  T* operator--() volatile noexcept { return fetch_sub(1) - 1; }
  T* operator--() noexcept { return fetch_sub(1) - 1; }
  T* operator+=(ptrdiff_t op) volatile noexcept { return fetch_add(op) + op; }
  T* operator+=(ptrdiff_t op) noexcept { return fetch_add(op) + op; }
  T* operator-=(ptrdiff_t op) volatile noexcept { return fetch_sub(op) - op; }
  T* operator-=(ptrdiff_t op) noexcept { return fetch_sub(op) - op; }
};

// named typedefs
typedef atomic<bool> atomic_bool;
typedef atomic<char> atomic_char;
typedef atomic<signed char> atomic_schar;
typedef atomic<unsigned char> atomic_uchar;
typedef atomic<short> atomic_short;
typedef atomic<unsigned short> atomic_ushort;
typedef atomic<int> atomic_int;
typedef atomic<unsigned int> atomic_uint;
typedef atomic<long> atomic_long;
typedef atomic<unsigned long> atomic_ulong;
typedef atomic<long long> atomic_llong;
typedef atomic<unsigned long long> atomic_ullong;
typedef atomic<char16_t> atomic_char16_t;
typedef atomic<char32_t> atomic_char32_t;
typedef atomic<wchar_t> atomic_wchar_t;
typedef atomic<int_least8_t> atomic_int_least8_t;
typedef atomic<uint_least8_t> atomic_uint_least8_t;
typedef atomic<int_least16_t> atomic_int_least16_t;
typedef atomic<uint_least16_t> atomic_uint_least16_t;
typedef atomic<int_least32_t> atomic_int_least32_t;
typedef atomic<uint_least32_t> atomic_uint_least32_t;
typedef atomic<int_least64_t> atomic_int_least64_t;
typedef atomic<uint_least64_t> atomic_uint_least64_t;
typedef atomic<int_fast8_t> atomic_int_fast8_t;
typedef atomic<uint_fast8_t> atomic_uint_fast8_t;
typedef atomic<int_fast16_t> atomic_int_fast16_t;
typedef atomic<uint_fast16_t> atomic_uint_fast16_t;
typedef atomic<int_fast32_t> atomic_int_fast32_t;
typedef atomic<uint_fast32_t> atomic_uint_fast32_t;
typedef atomic<int_fast64_t> atomic_int_fast64_t;
typedef atomic<uint_fast64_t> atomic_uint_fast64_t;
typedef atomic<intptr_t> atomic_intptr_t;
typedef atomic<uintptr_t> atomic_uintptr_t;
typedef atomic<size_t> atomic_size_t;
typedef atomic<ptrdiff_t> atomic_ptrdiff_t;
typedef atomic<intmax_t> atomic_intmax_t;
typedef atomic<uintmax_t> atomic_uintmax_t;

// general operations on atomic types
template <typename T>
bool atomic_is_lock_free(const volatile atomic<T>* a) noexcept {
  return a->is_lock_free();
}
template <typename T>
void atomic_is_lock_free(const atomic<T>* a) noexcept {
  return a->is_lock_free();
}
template <typename T>
void atomic_init(volatile atomic<T>* a, T d) noexcept {
  a->store(d);
}
template <typename T>
bool atomic_init(atomic<T>* a, T d) noexcept {
  a->store(d);
}
template <typename T>
void atomic_store(volatile atomic<T>* a, T d) noexcept {
  a->store(d);
}
template <typename T>
void atomic_store(atomic<T>* a, T d) noexcept {
  a->store(d);
}
template <typename T>
void atomic_store_explicit(volatile atomic<T>* a,
                           T d,
                           memory_order mo) noexcept {
  a->store(d, mo);
}
template <typename T>
void atomic_store_explicit(atomic<T>* a, T d, memory_order mo) noexcept {
  a->store(d, mo);
}
template <typename T>
T atomic_load(const volatile atomic<T>* a) noexcept {
  return a->load();
}
template <typename T>
T atomic_load(const atomic<T>* a) noexcept {
  return a->load();
}
template <typename T>
T atomic_load_explicit(const volatile atomic<T>* a, memory_order mo) noexcept {
  return a->load(mo);
}
template <typename T>
T atomic_load_explicit(const atomic<T>* a, memory_order mo) noexcept {
  return a->load(mo);
}
template <typename T>
T atomic_exchange(volatile atomic<T>* a, T d) noexcept {
  return a->exchange(d);
}
template <typename T>
T atomic_exchange(atomic<T>* a, T d) noexcept {
  return a->exchange(d);
}
template <typename T>
T atomic_echange_explicit(volatile atomic<T>* a,
                          T d,
                          memory_order mo) noexcept {
  return a->exchange(d, mo);
}
template <typename T>
T atomic_exchange_explicit(atomic<T>* a, T d, memory_order mo) noexcept {
  return a->exchange(d, mo);
}
template <typename T>
bool atomic_compare_exchange_weak(volatile atomic<T>* a, T* d, T e) noexcept {
  return a->compare_exchange_weak(*d, e);
}
template <typename T>
bool atomic_compare_exchange_weak(atomic<T>* a, T* d, T e) noexcept {
  return a->compare_exchange_weak(*d, e);
}
template <typename T>
bool atomic_compare_exchange_strong(volatile atomic<T>* a, T* d, T e) noexcept {
  return a->compare_exchange_strong(*d, e);
}
template <typename T>
bool atomic_compare_exchange_strong(atomic<T>* a, T* d, T e) noexcept {
  return a->compare_exchange_strong(*d, e);
}
template <typename T>
bool atomic_compare_exchange_weak_explicit(volatile atomic<T>* a,
                                           T* d,
                                           T e,
                                           memory_order so,
                                           memory_order fo) noexcept {
  return a->compare_exchange_weak(*d, e, so, fo);
}
template <typename T>
bool atomic_compare_exchange_weak_explicit(
    atomic<T>* a, T* d, T e, memory_order so, memory_order fo) noexcept {
  return a->compare_exchange_weak(*d, e, so, fo);
}
template <typename T>
bool atomic_compare_exchange_strong_explicit(volatile atomic<T>* a,
                                             T* d,
                                             T e,
                                             memory_order so,
                                             memory_order fo) noexcept {
  return a->compare_exchange_strong(*d, e, so, fo);
}
template <typename T>
bool atomic_compare_exchange_strong_explicit(
    atomic<T>* a, T* d, T e, memory_order so, memory_order fo) noexcept {
  return a->compare_exchange_strong(*d, e, so, fo);
}

template <typename T>
T atomic_fetch_add(volatile __infer_atomic_integral<T>* a, T i) noexcept {
  return a->fetch_add(i);
}
template <typename T>
T atomic_fetch_add(__infer_atomic_integral<T>* a, T i) noexcept {
  return a->fetch_add(i);
}
template <typename T>
T atomic_fetch_add_explicit(volatile __infer_atomic_integral<T>* a,
                            T i,
                            memory_order mo) noexcept {
  return a->fetch_add(i, mo);
}
template <typename T>
T atomic_fetch_add_explicit(__infer_atomic_integral<T>* a,
                            T i,
                            memory_order mo) noexcept {
  return a->fetch_add(i, mo);
}
template <typename T>
T atomic_fetch_sub(volatile __infer_atomic_integral<T>* a, T i) noexcept {
  return a->fetch_sub(i);
}
template <typename T>
T atomic_fetch_sub(__infer_atomic_integral<T>* a, T i) noexcept {
  return a->fetch_sub(i);
}
template <typename T>
T atomic_fetch_sub_explicit(volatile __infer_atomic_integral<T>* a,
                            T i,
                            memory_order mo) noexcept {
  return a->fetch_sub(i, mo);
}
template <typename T>
T atomic_fetch_sub_explicit(__infer_atomic_integral<T>* a,
                            T i,
                            memory_order mo) noexcept {
  return a->fetch_sub(i, mo);
}
template <typename T>
T atomic_fetch_and(volatile __infer_atomic_integral<T>* a, T i) noexcept {
  return a->fetch_and(i);
}
template <typename T>
T atomic_fetch_and(__infer_atomic_integral<T>* a, T i) noexcept {
  return a->fetch_and(i);
}
template <typename T>
T atomic_fetch_and_explicit(volatile __infer_atomic_integral<T>* a,
                            T i,
                            memory_order mo) noexcept {
  return a->fetch_and(i, mo);
}
template <typename T>
T atomic_fetch_and_explicit(__infer_atomic_integral<T>* a,
                            T i,
                            memory_order mo) noexcept {
  return a->fetch_and(i, mo);
}
template <typename T>
T atomic_fetch_or(volatile __infer_atomic_integral<T>* a, T i) noexcept {
  return a->fetch_or(i);
}
template <typename T>
T atomic_fetch_or(__infer_atomic_integral<T>* a, T i) noexcept {
  return a->fetch_or(i);
}
template <typename T>
T atomic_fetch_or_explicit(volatile __infer_atomic_integral<T>* a,
                           T i,
                           memory_order mo) noexcept {
  return a->fetch_or(i, mo);
}
template <typename T>
T atomic_fetch_or_explicit(__infer_atomic_integral<T>* a,
                           T i,
                           memory_order mo) noexcept {
  return a->fetch_or(i, mo);
}
template <typename T>
T atomic_fetch_xor(volatile __infer_atomic_integral<T>* a, T i) noexcept {
  return a->fetch_xor(i);
}
template <typename T>
T atomic_fetch_xor(__infer_atomic_integral<T>* a, T i) noexcept {
  return a->fetch_xor(i);
}
template <typename T>
T atomic_fetch_xor_explicit(volatile __infer_atomic_integral<T>* a,
                            T i,
                            memory_order mo) noexcept {
  return a->fetch_xor(i, mo);
}
template <typename T>
T atomic_fetch_xor_explicit(__infer_atomic_integral<T>* a,
                            T i,
                            memory_order mo) noexcept {
  return a->fetch_xor(i, mo);
}

// partial specialization for pointers
template <typename T>
T* atomic_fetch_add(volatile atomic<T*>* a, ptrdiff_t i) noexcept {
  return a->fetch_add(i);
}
template <typename T>
T* atomic_fetch_add(atomic<T*>* a, ptrdiff_t i) noexcept {
  return a->fetch_add(i);
}
template <typename T>
T* atomic_fetch_add_explicit(volatile atomic<T*>* a,
                             ptrdiff_t i,
                             memory_order mo) noexcept {
  return a->fetch_add(i, mo);
}
template <typename T>
T* atomic_fetch_add_explicit(atomic<T*>* a,
                             ptrdiff_t i,
                             memory_order mo) noexcept {
  return a->fetch_add(i, mo);
}
template <typename T>
T* atomic_fetch_sub(volatile atomic<T*>* a, ptrdiff_t i) noexcept {
  return a->fetch_sub(i);
}
template <typename T>
T* atomic_fetch_sub(atomic<T*>* a, ptrdiff_t i) noexcept {
  return a->fetch_sub(i);
}
template <typename T>
T* atomic_fetch_sub_explicit(volatile atomic<T*>* a,
                             ptrdiff_t i,
                             memory_order mo) noexcept {
  return a->fetch_sub(i, mo);
}
template <typename T>
T* atomic_fetch_sub_explicit(atomic<T*>* a,
                             ptrdiff_t i,
                             memory_order mo) noexcept {
  return a->fetch_sub(i, mo);
}

typedef struct atomic_flag {
  bool a;

  atomic_flag() noexcept = default;
  constexpr atomic_flag(bool i) noexcept : a(i) {}
  atomic_flag(const atomic_flag&) = delete;
  atomic_flag& operator=(const atomic_flag&) = delete;
  atomic_flag& operator=(const atomic_flag&) volatile = delete;

  bool test_and_set(memory_order mo = memory_order_seq_cst) volatile noexcept {
    bool ret = a;
    a = true;
    return ret;
  }
  bool test_and_set(memory_order mo = memory_order_seq_cst) noexcept {
    bool ret = a;
    a = true;
    return ret;
  }

  void clear(memory_order mo = memory_order_seq_cst) volatile noexcept {
    a = false;
  }
  void clear(memory_order mo = memory_order_seq_cst) noexcept { a = false; }

} atomic_flag;

bool atomic_flag_test_and_set(volatile atomic_flag* f) noexcept {
  return f->test_and_set();
}
bool atomic_flag_test_and_set(atomic_flag* f) noexcept {
  return f->test_and_set();
}
bool atomic_flag_test_and_set_explicit(volatile atomic_flag* f,
                                       memory_order m) noexcept {
  return f->test_and_set(m);
}
bool atomic_flag_test_and_set_explicit(atomic_flag* f,
                                       memory_order m) noexcept {
  return f->test_and_set(m);
}
void atomic_flag_clear(volatile atomic_flag* f) noexcept { f->clear(); }
void atomic_flag_clear(atomic_flag* f) noexcept { f->clear(); }
void atomic_flag_clear_explicit(volatile atomic_flag* f,
                                memory_order mo) noexcept {
  f->clear(mo);
}
void atomic_flag_clear_explicit(atomic_flag* f, memory_order mo) noexcept {
  f->clear(mo);
}

void atomic_thread_fence(memory_order mo) noexcept {}
void atomic_signal_fence(memory_order mo) noexcept {}

#define ATOMIC_FLAG_INIT \
  { false }
#define ATOMIC_VAR_INIT(__v) \
  { __v }

INFER_NAMESPACE_STD_END
