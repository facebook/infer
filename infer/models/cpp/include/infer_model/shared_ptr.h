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

// define shared_ptr outside of std namespace. It will be aliased as
// std::shared_ptr via 'using' clause later
namespace infer_std_model {

// use inheritance to avoid compilation errors when using
// methods / non-member functions that are not modeled
// WARNING: sizeof(shared_ptr) = 24, not 16 - this may
// lead to compilation errors
template <class T>
class shared_ptr : public std::std__shared_ptr<T> {

 public:
#if INFER_USE_LIBCPP
  using std::std__shared_ptr<T>::__ptr_;
#define __data __ptr_
#else
  using std::__shared_ptr<T>::_M_ptr;
#define __data _M_ptr
#endif
  // Conversion constructors to allow implicit conversions.
  // it's here purely to avoid compilation errors
  template <class Y,
            typename = typename std::enable_if<
                std::is_convertible<Y*, T*>::value>::type>
  shared_ptr(const std::std__shared_ptr<Y>& r) {}

  template <class Y>
  shared_ptr(const std::std__shared_ptr<Y>& r, T* p) noexcept {}

  // constructors:
  constexpr shared_ptr() noexcept { __data = nullptr; }

  shared_ptr(std::nullptr_t) : shared_ptr() {}

  // Extra template argument is used to create constructors/assignment overloads
  // for Y types where it's possible to convert Y* to T*.
  // typename = typename std::enable_if<std::is_convertible<Y*,
  // T*>::value>::type
  // thanks to that, clang will not create some functions that would cause
  // compilation errors. More info:
  // http://en.cppreference.com/w/cpp/language/sfinae
  template <class Y,
            typename = typename std::enable_if<
                std::is_convertible<Y*, T*>::value>::type>
  explicit shared_ptr(Y* p) {
    __data = p;
  }

  template <class Y,
            class D,
            typename = typename std::enable_if<
                std::is_convertible<Y*, T*>::value>::type>
  shared_ptr(Y* p, D d) : shared_ptr<T>(p) {}

  template <class Y,
            class D,
            class A,
            typename = typename std::enable_if<
                std::is_convertible<Y*, T*>::value>::type>
  shared_ptr(Y* p, D d, A a) : shared_ptr<T>(p) {}

  template <class D>
  shared_ptr(std::nullptr_t p, D d) : shared_ptr<T>(p) {}

  template <class D, class A>
  shared_ptr(std::nullptr_t p, D d, A a) : shared_ptr<T>(p) {}

  template <class Y>
  shared_ptr(const shared_ptr<Y>& r, T* p) noexcept {
    __data = nullptr; /* TODO */
  }

  shared_ptr(const shared_ptr& r) noexcept
      : shared_ptr<T>(r.__data) { /* TODO - increase refcount*/
  }

  template <class Y,
            typename = typename std::enable_if<
                std::is_convertible<Y*, T*>::value>::type>
  shared_ptr(const shared_ptr<Y>& r) noexcept
      : shared_ptr<T>(r.__data) { /* TODO - increase refcount*/
  }

  shared_ptr(shared_ptr&& r) noexcept : shared_ptr<T>(r.__data) {
    r.__data = nullptr;
  }

  template <class Y,
            typename = typename std::enable_if<
                std::is_convertible<Y*, T*>::value>::type>
  shared_ptr(shared_ptr<Y>&& r) noexcept : shared_ptr<T>(r.__data) {
    r.__data = nullptr;
  }

  template <class Y,
            typename = typename std::enable_if<
                std::is_convertible<Y*, T*>::value>::type>
  explicit shared_ptr(const std::weak_ptr<Y>& r) {}

  /* Because of implementation differences between libc++ and stdlibc++, don't
   * define this constructor (it will be defined elsewhere in case of
   * stdlibc++). Because it may be defined elsewhere, don't check whether Y*
   * converts to T* - otherwise there might be compilation error (out-of-line
   * definition).
   * No definition here might cause compilation problems if project is
   * using std::auto_ptrs with libc++ */
  template <class Y>
  shared_ptr(std::auto_ptr<Y>&& r); // {}

  template <class Y,
            class D,
            typename = typename std::enable_if<
                std::is_convertible<Y*, T*>::value>::type>
  shared_ptr(std::unique_ptr<Y, D>&& r) : shared_ptr<T>(r.release()) {}

  // destructor:
  ~shared_ptr() { reset((T*)nullptr); }

  // assignment:
  shared_ptr& operator=(const shared_ptr& r) noexcept {
    // shared_ptr<T>(r).swap(*this);
    __data = r.__data;
    return *this;
  }

  template <class Y,
            typename = typename std::enable_if<
                std::is_convertible<Y*, T*>::value>::type>
  shared_ptr& operator=(const shared_ptr<Y>& r) noexcept {
    // shared_ptr<T>(r).swap(*this);
    __data = r.__data;
    return *this;
  }

  shared_ptr& operator=(shared_ptr&& r) noexcept {
    // shared_ptr<T>(std::move(r)).swap(*this);
    __data = r.__data;
    return *this;
  }

  template <class Y,
            typename = typename std::enable_if<
                std::is_convertible<Y*, T*>::value>::type>
  shared_ptr& operator=(shared_ptr<Y>&& r) {
    // shared_ptr<T>(std::move(r)).swap(*this);
    __data = r.__data;
    return *this;
  }

  template <class Y,
            typename = typename std::enable_if<
                std::is_convertible<Y*, T*>::value>::type>
  shared_ptr& operator=(std::auto_ptr<Y>&& r) { /* ?? */
  }
  template <class Y,
            class D,
            typename = typename std::enable_if<
                std::is_convertible<Y*, T*>::value>::type>
  shared_ptr& operator=(std::unique_ptr<Y, D>&& r) {
    // shared_ptr<T>(std::move(r)).swap(*this);
    return *this;
  }

  // modifiers:
  void swap(shared_ptr& r) noexcept {
    T* tmp = r.__data;
    r.__data = __data;
    __data = tmp;
  }

  void reset() noexcept { reset((T*)nullptr); }

  template <class Y,
            typename = typename std::enable_if<
                std::is_convertible<Y*, T*>::value>::type>
  void reset(Y* p) {
    /*
    if (unique()) {
      delete __data;
    }
    */
    __data = p;
    // TODO adjust refcounts
  }

  template <class Y,
            class D,
            typename = typename std::enable_if<
                std::is_convertible<Y*, T*>::value>::type>
  void reset(Y* p, D d) {
    reset(p);
  }

  template <class Y,
            class D,
            class A,
            typename = typename std::enable_if<
                std::is_convertible<Y*, T*>::value>::type>
  void reset(Y* p, D d, A a) {
    reset(p);
  }

  // observers:
  T* get() const noexcept { return __data; }
  typename std::add_lvalue_reference<T>::type operator*() const noexcept {
    return *__data;
  }
  T* operator->() const noexcept { return __data; }
  long use_count() const noexcept { return 2; /* FIXME */ }
  bool unique() const noexcept { return use_count() == 1; /* FIXME */ }
  explicit operator bool() const noexcept { return (bool)__data; }
  template <class U>
  bool owner_before(shared_ptr<U> const& b) const {
    return true; /* FIXME - use non-det*/
  }
  template <class U>
  bool owner_before(std::weak_ptr<U> const& b) const {
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
  typedef typename std::common_type<_Tp*, _Up*>::type _Vp;
  return std::less<_Vp>()(__x.get(), __y.get());
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
inline bool operator==(const shared_ptr<_Tp>& __x, std::nullptr_t) noexcept {
  return !__x;
}

template <class _Tp>
inline bool operator==(std::nullptr_t, const shared_ptr<_Tp>& __x) noexcept {
  return !__x;
}

template <class _Tp>
inline bool operator!=(const shared_ptr<_Tp>& __x, std::nullptr_t) noexcept {
  return static_cast<bool>(__x);
}

template <class _Tp>
inline bool operator!=(std::nullptr_t, const shared_ptr<_Tp>& __x) noexcept {
  return static_cast<bool>(__x);
}

template <class _Tp>
inline bool operator<(const shared_ptr<_Tp>& __x, std::nullptr_t) noexcept {
  return std::less<_Tp*>()(__x.get(), nullptr);
}

template <class _Tp>
inline bool operator<(std::nullptr_t, const shared_ptr<_Tp>& __x) noexcept {
  return std::less<_Tp*>()(nullptr, __x.get());
}

template <class _Tp>
inline bool operator>(const shared_ptr<_Tp>& __x, std::nullptr_t) noexcept {
  return nullptr < __x;
}

template <class _Tp>
inline bool operator>(std::nullptr_t, const shared_ptr<_Tp>& __x) noexcept {
  return __x < nullptr;
}

template <class _Tp>
inline bool operator<=(const shared_ptr<_Tp>& __x, std::nullptr_t) noexcept {
  return !(nullptr < __x);
}

template <class _Tp>
inline bool operator<=(std::nullptr_t, const shared_ptr<_Tp>& __x) noexcept {
  return !(__x < nullptr);
}

template <class _Tp>
inline bool operator>=(const shared_ptr<_Tp>& __x, std::nullptr_t) noexcept {
  return !(__x < nullptr);
}

template <class _Tp>
inline bool operator>=(std::nullptr_t, const shared_ptr<_Tp>& __x) noexcept {
  return !(nullptr < __x);
}
} // namespace infer_std_model

INFER_NAMESPACE_STD_BEGIN

// make std::shared_ptr alias of infer_std_model::shared_ptr
template <class T>
using shared_ptr = infer_std_model::shared_ptr<T>;

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

#undef __data
INFER_NAMESPACE_STD_END
