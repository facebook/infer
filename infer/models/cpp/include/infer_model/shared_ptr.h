/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#pragma once
// ASSERT that __cplusplus >= 201103L

#include <infer_model/common.h>

INFER_NAMESPACE_STD_BEGIN

// use inheritance to avoid compilation errors when using
// methods / non-member functions that are not modeled
// WARNING: sizeof(shared_ptr) = 24, not 16 - this may
// lead to compilation errors
template <class T>
class shared_ptr : public std__shared_ptr<T> {

 public:
  // Conversion constructors to allow implicit conversions.
  // it's here purely to avoid compilation errors
  template <class Y,
            typename = typename enable_if<is_convertible<Y*, T*>::value>::type>
  shared_ptr(const std__shared_ptr<Y>& r) {}

  template <class Y>
  shared_ptr(const std__shared_ptr<Y>& r, T* p) noexcept {}

  T* data;

  // constructors:
  constexpr shared_ptr() noexcept : data(nullptr) {}

  shared_ptr(nullptr_t) : shared_ptr() {}

  // Extra template argument is used to create constructors/assignment overloads
  // for Y types where it's possible to convert Y* to T*.
  // typename = typename enable_if<is_convertible<Y*, T*>::value>::type
  // thanks to that, clang will not create some functions that would cause
  // compilation errors. More info:
  // http://en.cppreference.com/w/cpp/language/sfinae
  template <class Y,
            typename = typename enable_if<is_convertible<Y*, T*>::value>::type>
  explicit shared_ptr(Y* p) {
    data = p;
  }

  template <class Y,
            class D,
            typename = typename enable_if<is_convertible<Y*, T*>::value>::type>
  shared_ptr(Y* p, D d) : shared_ptr<T>(p) {}

  template <class Y,
            class D,
            class A,
            typename = typename enable_if<is_convertible<Y*, T*>::value>::type>
  shared_ptr(Y* p, D d, A a) : shared_ptr<T>(p) {}

  template <class D>
  shared_ptr(nullptr_t p, D d) : shared_ptr<T>(p) {}

  template <class D, class A>
  shared_ptr(nullptr_t p, D d, A a) : shared_ptr<T>(p) {}

  template <class Y>
  shared_ptr(const shared_ptr<Y>& r, T* p) noexcept : data(nullptr) { /* TODO */
  }

  shared_ptr(const shared_ptr& r) noexcept
      : shared_ptr<T>(r.data) { /* TODO - increase refcount*/
  }

  template <class Y,
            typename = typename enable_if<is_convertible<Y*, T*>::value>::type>
  shared_ptr(const shared_ptr<Y>& r) noexcept
      : shared_ptr<T>(r.data) { /* TODO - increase refcount*/
  }

  shared_ptr(shared_ptr&& r) noexcept : shared_ptr<T>(r.data) {
    r.data = nullptr;
  }

  template <class Y,
            typename = typename enable_if<is_convertible<Y*, T*>::value>::type>
  shared_ptr(shared_ptr<Y>&& r) noexcept : shared_ptr<T>(r.data) {
    r.data = nullptr;
  }

  template <class Y,
            typename = typename enable_if<is_convertible<Y*, T*>::value>::type>
  explicit shared_ptr(const weak_ptr<Y>& r) {}

  /* Because of implementation differences between libc++ and stdlibc++, don't
   * define this constructor (it will be defined elsewhere in case of
   * stdlibc++). Because it may be defined elsewhere, don't check whether Y*
   * converts to T* - otherwise there might be compilation error (out-of-line
   * definition).
   * No definition here might cause compilation problems if project is
   * using auto_ptrs with libc++ */
  template <class Y>
  shared_ptr(auto_ptr<Y>&& r); // {}

  template <class Y,
            class D,
            typename = typename enable_if<is_convertible<Y*, T*>::value>::type>
  shared_ptr(unique_ptr<Y, D>&& r) : shared_ptr<T>(r.release()) {}

  // destructor:
  ~shared_ptr() { reset((T*)nullptr); }

  // assignment:
  shared_ptr& operator=(const shared_ptr& r) noexcept {
    // shared_ptr<T>(r).swap(*this);
    data = r.data;
    return *this;
  }

  template <class Y,
            typename = typename enable_if<is_convertible<Y*, T*>::value>::type>
  shared_ptr& operator=(const shared_ptr<Y>& r) noexcept {
    // shared_ptr<T>(r).swap(*this);
    data = r.data;
    return *this;
  }

  shared_ptr& operator=(shared_ptr&& r) noexcept {
    // shared_ptr<T>(std::move(r)).swap(*this);
    data = r.data;
    return *this;
  }

  template <class Y,
            typename = typename enable_if<is_convertible<Y*, T*>::value>::type>
  shared_ptr& operator=(shared_ptr<Y>&& r) {
    // shared_ptr<T>(std::move(r)).swap(*this);
    data = r.data;
    return *this;
  }

  template <class Y,
            typename = typename enable_if<is_convertible<Y*, T*>::value>::type>
  shared_ptr& operator=(auto_ptr<Y>&& r) { /* ?? */
  }
  template <class Y,
            class D,
            typename = typename enable_if<is_convertible<Y*, T*>::value>::type>
  shared_ptr& operator=(unique_ptr<Y, D>&& r) {
    // shared_ptr<T>(std::move(r)).swap(*this);
    return *this;
  }

  // modifiers:
  void swap(shared_ptr& r) noexcept {
    T* tmp = r.data;
    r.data = data;
    data = tmp;
  }

  void reset() noexcept { reset((T*)nullptr); }

  template <class Y,
            typename = typename enable_if<is_convertible<Y*, T*>::value>::type>
  void reset(Y* p) {
    /*
    if (unique()) {
      delete data;
    }
    */
    data = p;
    // TODO adjust refcounts
  }

  template <class Y,
            class D,
            typename = typename enable_if<is_convertible<Y*, T*>::value>::type>
  void reset(Y* p, D d) {
    reset(p);
  }

  template <class Y,
            class D,
            class A,
            typename = typename enable_if<is_convertible<Y*, T*>::value>::type>
  void reset(Y* p, D d, A a) {
    reset(p);
  }

  // observers:
  T* get() const noexcept { return data; }
  typename add_lvalue_reference<T>::type operator*() const noexcept {
    return *data;
  }
  T* operator->() const noexcept { return data; }
  long use_count() const noexcept { return 2; /* FIXME */ }
  bool unique() const noexcept { return use_count() == 1; /* FIXME */ }
  explicit operator bool() const noexcept { return (bool)data; }
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
struct hash<shared_ptr<T>> : public hash<std__shared_ptr<T>> {};

// shared_ptr casts - call original functions but change return type to
// std::shared_ptr
template <class T, class U>
shared_ptr<T> static_pointer_cast(shared_ptr<U> const& r) noexcept {
  return static_pointer_cast<T, U>((const std__shared_ptr<U>&)r);
}
template <class T, class U>
shared_ptr<T> dynamic_pointer_cast(shared_ptr<U> const& r) noexcept {
  return dynamic_pointer_cast<T, U>((const std__shared_ptr<U>&)r);
}
template <class T, class U>
shared_ptr<T> const_pointer_cast(shared_ptr<U> const& r) noexcept {
  return const_pointer_cast<T, U>((const std__shared_ptr<U>&)r);
}

template <class T, class... Args>
shared_ptr<T> make_shared(Args&&... args) {
  return shared_ptr<T>(new T(std::forward<Args>(args)...));
}

INFER_NAMESPACE_STD_END
