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
#include <infer_model/infer_traits.h>
#include <infer_model/weak_ptr.h>

INFER_NAMESPACE_STD_BEGIN

// use inheritance to avoid compilation errors when using
// methods / non-member functions that are not modeled
// WARNING: if sizeof(shared_ptr) becomes different than 16, it may
// lead to compilation errors
template <class T>
class shared_ptr : public std__shared_ptr<T> {

  // translate shared_ptr as type T*
  friend class infer_traits::TranslateAsType<T*>;

  // shared_ptr<T> in infer is translated as T*.
  // Some facts:
  // 1. shared_ptr<T>* translated as T**
  // 2. typeof(this) translated as T**
  // 3. typeof(this) in clang's AST is shared_ptr<T>*
  // When writing models for shared_ptr, we need to use infer's representation
  // In order to achieve that and not break compilation, there is some ugly
  // casting going around. We are using void* and void** to make compilation
  // happy - infer doesn't care about those types that much since they are
  // pointers anyway.
  // Example of model_X function declaration:
  // static void model_X(infer_shared_ptr_t self, ... params)
  // model_X are really C functions, but to simplify linking process, they are
  // defined inside shared_ptr class as static methods.
  // When using model_X functions, call them like so:
  //    model_X(__cast_to_infer_ptr(this), args)

  /// type of 'this' in shared_ptr<T> as seen by infer
  typedef const void** infer_shared_ptr_t;
// use it to avoid compilation errors and make infer analyzer happy
#define __cast_to_infer_ptr(self) ((infer_shared_ptr_t)self)

  // provide overload for volatile void* to accomodate for situation when
  // T is volatile ('volatile int' for example). 'void*' and 'nullptr_t'
  // overloads are to avoid 'call to model_set is ambiguous' compilation errors
  static void model_set(infer_shared_ptr_t self, nullptr_t value) {
    *self = value;
  }

  static void model_set(infer_shared_ptr_t self, const void* value) {
    *self = value;
  }

  static void model_set(infer_shared_ptr_t self, volatile void* value) {
    *self = const_cast<const void*>(value);
  }

  static void model_set(infer_shared_ptr_t self, void* value) {
    *self = const_cast<const void*>(value);
  }

  static void model_copy(infer_shared_ptr_t self, infer_shared_ptr_t other) {
    /* TODO - increase refcount*/
    *self = *other;
  }

  static void model_move(infer_shared_ptr_t self, infer_shared_ptr_t other) {
    model_copy(self, other);
    model_set(other, nullptr);
  }

  static T* model_get(infer_shared_ptr_t self) { return (T*)(*self); }

  static void model_swap(infer_shared_ptr_t infer_self,
                         infer_shared_ptr_t infer_other) {
    const void* t = *infer_self;
    *infer_self = *infer_other;
    *infer_other = t;
  }

 public:
  // Conversion constructors to allow implicit conversions.
  // it's here purely to avoid compilation errors
  template <class Y,
            typename = typename enable_if<is_convertible<Y*, T*>::value>::type>
  shared_ptr(const std__shared_ptr<Y>& r) {}

  template <class Y>
  shared_ptr(const std__shared_ptr<Y>& r, T* p) noexcept {}

  // constructors:
  constexpr shared_ptr() noexcept {
    model_set(__cast_to_infer_ptr(this), nullptr);
  }

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
    model_set(__cast_to_infer_ptr(this), p);
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
  shared_ptr(const shared_ptr<Y>& r, T* p) noexcept {
    model_set(__cast_to_infer_ptr(this), p); /* TODO */
  }

  shared_ptr(const shared_ptr& r) noexcept {
    model_copy(__cast_to_infer_ptr(this), __cast_to_infer_ptr(&r));
  }

  template <class Y,
            typename = typename enable_if<is_convertible<Y*, T*>::value>::type>
  shared_ptr(const shared_ptr<Y>& r) noexcept {
    model_copy(__cast_to_infer_ptr(this), __cast_to_infer_ptr(&r));
  }

  shared_ptr(shared_ptr&& r) noexcept {
    model_move(__cast_to_infer_ptr(this), __cast_to_infer_ptr(&r));
  }

  template <class Y,
            typename = typename enable_if<is_convertible<Y*, T*>::value>::type>
  shared_ptr(shared_ptr<Y>&& r) noexcept {
    model_move(__cast_to_infer_ptr(this), __cast_to_infer_ptr(&r));
  }

  template <class Y,
            typename = typename enable_if<is_convertible<Y*, T*>::value>::type>
  explicit shared_ptr(const weak_ptr<Y>& r) : shared_ptr(std::move(r.lock())) {
    // TODO: throw if r is empty
  }

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
    model_copy(__cast_to_infer_ptr(this), __cast_to_infer_ptr(&r));
    return *this;
  }

  template <class Y,
            typename = typename enable_if<is_convertible<Y*, T*>::value>::type>
  shared_ptr& operator=(const shared_ptr<Y>& r) noexcept {
    // shared_ptr<T>(r).swap(*this);
    model_copy(__cast_to_infer_ptr(this), __cast_to_infer_ptr(&r));
    return *this;
  }

  shared_ptr& operator=(shared_ptr&& r) noexcept {
    // shared_ptr<T>(std::move(r)).swap(*this);
    model_move(__cast_to_infer_ptr(this), __cast_to_infer_ptr(&r));
    return *this;
  }

  template <class Y,
            typename = typename enable_if<is_convertible<Y*, T*>::value>::type>
  shared_ptr& operator=(shared_ptr<Y>&& r) {
    // shared_ptr<T>(std::move(r)).swap(*this);
    model_move(__cast_to_infer_ptr(this), __cast_to_infer_ptr(&r));
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
    model_swap(__cast_to_infer_ptr(this), __cast_to_infer_ptr(&r));
  }

  void reset() noexcept { reset((T*)nullptr); }

  template <class Y,
            typename = typename enable_if<is_convertible<Y*, T*>::value>::type>
  void reset(Y* p) {
    /*
    if (unique()) {
      delete __data;
    }
    */
    model_set(__cast_to_infer_ptr(this), p);
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
  T* get() const noexcept INFER_MODEL_AS_DEREF_FIRST_ARG;

  typename std::add_lvalue_reference<T>::type operator*() const
      noexcept INFER_MODEL_AS_DEREF_FIRST_ARG;

  T* operator->() const noexcept INFER_MODEL_AS_DEREF_FIRST_ARG;
  long use_count() const noexcept { return 2; /* FIXME */ }
  bool unique() const noexcept { return use_count() == 1; /* FIXME */ }
  explicit operator bool() const noexcept {
    // for some reason analyzer can't cast to bool correctly, trick with two
    // negations creates right specs for this function
    return !!(bool)(model_get(__cast_to_infer_ptr(this)));
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
template <class _Tp, class _Up>
inline bool operator==(const shared_ptr<_Tp>& __x,
                       const shared_ptr<_Up>& __y) noexcept {
  return __x.get() == __y.get();
}

template <class _Tp, class _Up>
inline bool operator!=(const shared_ptr<_Tp>& __x,
                       const shared_ptr<_Up>& __y) noexcept {
  return !(__x == __y);
}

template <class _Tp, class _Up>
inline bool operator<(const shared_ptr<_Tp>& __x,
                      const shared_ptr<_Up>& __y) noexcept {
  typedef typename common_type<_Tp*, _Up*>::type _Vp;
  return less<_Vp>()(__x.get(), __y.get());
}

template <class _Tp, class _Up>
inline bool operator>(const shared_ptr<_Tp>& __x,
                      const shared_ptr<_Up>& __y) noexcept {
  return __y < __x;
}

template <class _Tp, class _Up>
inline bool operator<=(const shared_ptr<_Tp>& __x,
                       const shared_ptr<_Up>& __y) noexcept {
  return !(__y < __x);
}

template <class _Tp, class _Up>
inline bool operator>=(const shared_ptr<_Tp>& __x,
                       const shared_ptr<_Up>& __y) noexcept {
  return !(__x < __y);
}

template <class _Tp>
inline bool operator==(const shared_ptr<_Tp>& __x, nullptr_t) noexcept {
  return !__x;
}

template <class _Tp>
inline bool operator==(nullptr_t, const shared_ptr<_Tp>& __x) noexcept {
  return !__x;
}

template <class _Tp>
inline bool operator!=(const shared_ptr<_Tp>& __x, nullptr_t) noexcept {
  return static_cast<bool>(__x);
}

template <class _Tp>
inline bool operator!=(nullptr_t, const shared_ptr<_Tp>& __x) noexcept {
  return static_cast<bool>(__x);
}

template <class _Tp>
inline bool operator<(const shared_ptr<_Tp>& __x, nullptr_t) noexcept {
  return less<_Tp*>()(__x.get(), nullptr);
}

template <class _Tp>
inline bool operator<(nullptr_t, const shared_ptr<_Tp>& __x) noexcept {
  return less<_Tp*>()(nullptr, __x.get());
}

template <class _Tp>
inline bool operator>(const shared_ptr<_Tp>& __x, nullptr_t) noexcept {
  return nullptr < __x;
}

template <class _Tp>
inline bool operator>(nullptr_t, const shared_ptr<_Tp>& __x) noexcept {
  return __x < nullptr;
}

template <class _Tp>
inline bool operator<=(const shared_ptr<_Tp>& __x, nullptr_t) noexcept {
  return !(nullptr < __x);
}

template <class _Tp>
inline bool operator<=(nullptr_t, const shared_ptr<_Tp>& __x) noexcept {
  return !(__x < nullptr);
}

template <class _Tp>
inline bool operator>=(const shared_ptr<_Tp>& __x, nullptr_t) noexcept {
  return !(__x < nullptr);
}

template <class _Tp>
inline bool operator>=(nullptr_t, const shared_ptr<_Tp>& __x) noexcept {
  return !(nullptr < __x);
}

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

template <class T>
class enable_shared_from_this : public std__enable_shared_from_this<T> {
 public:
  shared_ptr<T> shared_from_this() {
    return std__enable_shared_from_this<T>::shared_from_this();
  }
  shared_ptr<T const> shared_from_this() const {
    return std__enable_shared_from_this<T>::shared_from_this();
  }
};

template <class T, class... Args>
shared_ptr<T> make_shared(Args&&... args) {
  return shared_ptr<T>(::new T(std::forward<Args>(args)...));
}

template <class T>
struct owner_less;

template <class T>
struct owner_less<shared_ptr<T>>
    : binary_function<shared_ptr<T>, shared_ptr<T>, bool> {
  typedef bool result_type;

  bool operator()(shared_ptr<T> const& x, shared_ptr<T> const& y) const {
    return x.owner_before(y);
  }

  bool operator()(shared_ptr<T> const& x, weak_ptr<T> const& y) const {
    return x.owner_before(y);
  }

  bool operator()(weak_ptr<T> const& x, shared_ptr<T> const& y) const {
    return x.owner_before(y);
  }
};

#undef __cast_to_infer_ptr
INFER_NAMESPACE_STD_END
