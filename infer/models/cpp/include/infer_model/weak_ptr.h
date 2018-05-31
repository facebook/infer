/*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

#include <infer_model/common.h>
#include <infer_model/infer_traits.h>

unsigned long int __infer_nondet_unsigned_long_int();

INFER_NAMESPACE_STD_BEGIN

// forward declaration because it is used here
template <class T>
class shared_ptr;

// Ideally: : public std__weak_ptr<T>
// use inheritance to avoid compilation errors when using
// methods / non-member functions that are not modeled
// Currently not inherited because it leads to Symexec_memory_error

template <class T>
class weak_ptr {

  template <class>
  friend class weak_ptr;

  // WARNING: if sizeof(weak_ptr) becomes different than 16, it may
  // lead to compilation errors
  T* ptr;
  void* __ignore;

 public:
  // Conversion constructors to allow implicit conversions.
  // it's here purely to avoid compilation errors
  template <class Y,
            typename = typename enable_if<is_convertible<Y*, T*>::value>::type>
  weak_ptr(const std__weak_ptr<Y>& r) {}

  // constructors:
  constexpr weak_ptr() noexcept { ptr = nullptr; }

  template <class Y,
            typename = typename enable_if<is_convertible<Y*, T*>::value>::type>
  weak_ptr(const shared_ptr<Y>& r) noexcept {
    ptr = r.get();
  }

  weak_ptr(const weak_ptr& r) noexcept { ptr = r.ptr; }

  template <class Y,
            typename = typename enable_if<is_convertible<Y*, T*>::value>::type>
  weak_ptr(const weak_ptr<Y>& r) noexcept {
    ptr = r.ptr;
  }

  weak_ptr(weak_ptr&& r) noexcept {
    ptr = r.ptr;
    r.ptr = nullptr;
  }

  template <class Y,
            typename = typename enable_if<is_convertible<Y*, T*>::value>::type>
  weak_ptr(weak_ptr<Y>&& r) noexcept {
    ptr = r.ptr;
    r.ptr = nullptr;
  }

  // destructor:
  ~weak_ptr() { ptr = nullptr; }

  // assignment:
  weak_ptr& operator=(const weak_ptr& r) noexcept {
    // weak_ptr<T>(r).swap(*this);
    ptr = r.ptr;
    return *this;
  }

  template <class Y,
            typename = typename enable_if<is_convertible<Y*, T*>::value>::type>
  weak_ptr& operator=(const weak_ptr<Y>& r) noexcept {
    // weak_ptr<T>(r).swap(*this);
    ptr = r.ptr;
    return *this;
  }

  template <class Y,
            typename = typename enable_if<is_convertible<Y*, T*>::value>::type>
  weak_ptr& operator=(const shared_ptr<Y>& r) noexcept {
    // weak_ptr<T>(r).swap(*this);
    ptr = r.get();
    return *this;
  }

  weak_ptr& operator=(weak_ptr&& r) noexcept {
    // shared_ptr<T>(std::move(r)).swap(*this);
    ptr = r.ptr;
    r.ptr = nullptr;
    return *this;
  }

  template <class Y,
            typename = typename enable_if<is_convertible<Y*, T*>::value>::type>
  weak_ptr& operator=(weak_ptr<Y>&& r) {
    // weak_ptr<T>(std::move(r)).swap(*this);
    ptr = r.ptr;
    r.ptr = nullptr;
    return *this;
  }

  // modifiers:
  void swap(weak_ptr& r) noexcept {
    T* tmp = ptr;
    ptr = r.ptr;
    r.ptr = tmp;
  }

  void reset() noexcept {
    // weak_ptr().swap(*this);
    ptr = nullptr;
  }

  // observers:
  long use_count() const noexcept {
    if (ptr) {
      return __infer_nondet_unsigned_long_int();
    }
    return 0;
  }

  bool expired() const noexcept { return use_count() <= 0; }

  shared_ptr<T> lock() const noexcept {
    if (use_count() > 0) {
      return shared_ptr<T>(ptr);
    }
    return shared_ptr<T>();
  }

  template <class U>
  bool owner_before(shared_ptr<U> const& b) const {
    return true; /* FIXME - use non-det*/
  }
  template <class U>
  bool owner_before(weak_ptr<U> const& b) const {
    return true; /* FIXME - use non-det */
  }
};

template <class T>
struct owner_less;

template <class T>
struct owner_less<weak_ptr<T>>
    : binary_function<weak_ptr<T>, weak_ptr<T>, bool> {
  typedef bool result_type;
  bool operator()(weak_ptr<T> const& x, weak_ptr<T> const& y) const {
    return x.owner_before(y);
  }

  bool operator()(shared_ptr<T> const& x, weak_ptr<T> const& y) const {
    return x.owner_before(y);
  }

  bool operator()(weak_ptr<T> const& x, shared_ptr<T> const& y) const {
    return x.owner_before(y);
  }
};

INFER_NAMESPACE_STD_END
