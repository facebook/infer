/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#pragma once

#include <infer_model/common.h>
#include <infer_model/infer_traits.h>

INFER_NAMESPACE_STD_BEGIN

// IMPORTANT
// There is specialization of unique_ptr below and it's mosly copy paste.
// When changing model, remember to change it for specialization as well!
template <class _Tp, class _Dp = default_delete<_Tp>>
struct unique_ptr {

  // use SFINAE to determine whether _Del::pointer exists
  class _Pointer {
    template <typename _Up>
    static typename _Up::pointer __test(typename _Up::pointer*);

    template <typename _Up>
    static _Tp* __test(...);

    typedef typename remove_reference<_Dp>::type _Del;

   public:
    typedef decltype(__test<_Del>(0)) type;
  };
 public:
  typedef typename _Pointer::type pointer;
  typedef _Tp element_type;
  typedef _Dp deleter_type;

 private:
  /* std::unique_ptr<T> in infer is translated as T*
     Look at model of std::shared_ptr for more details */

  // translate shared_ptr as type 'pointer'
  friend class infer_traits::TranslateAsType<pointer>;

  /// type of 'this' in unique_ptr<T> as seen by infer
  typedef const void** infer_unique_ptr_t;
// use it to avoid compilation errors and make infer analyzer happy
#define __cast_to_infer_ptr(self) ((infer_unique_ptr_t)self)

  // provide overload for volatile void* to accommodate for situation when
  // T is volatile ('volatile int' for example). 'void*' and 'nullptr_t'
  // overloads are to avoid 'call to model_set is ambiguous' compilation errors
  static void model_set(infer_unique_ptr_t self, nullptr_t value) noexcept {
    *self = value;
  }

  static void model_set(infer_unique_ptr_t self, const void* value) noexcept {
    *self = value;
  }

  static void model_set(infer_unique_ptr_t self,
                        volatile void* value) noexcept {
    *self = const_cast<const void*>(value);
  }

  static void model_set(infer_unique_ptr_t self, void* value) noexcept {
    *self = const_cast<const void*>(value);
  }

  // in case 'pointer' type is not type pointer, enable catch all overload of
  // `model_set` to prevent compliation issues
  static void model_set(...) noexcept { /* no model for this overload */
  }

  static void model_move(infer_unique_ptr_t self,
                         infer_unique_ptr_t other) noexcept {
    *self = *other;
    model_set(other, nullptr);
  }

  static void model_swap(infer_unique_ptr_t infer_self,
                         infer_unique_ptr_t infer_other) noexcept {
    const void* t = *infer_self;
    *infer_self = *infer_other;
    *infer_other = t;
  }

  pointer __ignore__; // used to keep sizeof(unique_ptr) same as in standard

 public:
  template <class Y>
  unique_ptr(const std__unique_ptr<Y>& u) {}

  constexpr unique_ptr() noexcept {
    model_set(__cast_to_infer_ptr(this), nullptr);
  }

  constexpr unique_ptr(nullptr_t) noexcept : unique_ptr<_Tp, _Dp>() {}

  explicit unique_ptr(pointer ptr) noexcept {
    model_set(__cast_to_infer_ptr(this), ptr);
  }

  unique_ptr(pointer ptr,
             typename conditional<
                 is_reference<deleter_type>::value,
                 deleter_type,
                 typename add_lvalue_reference<const deleter_type>::type>::type
                 __d) noexcept
      : unique_ptr<_Tp, _Dp>(ptr) {}

  unique_ptr(pointer ptr,
             typename remove_reference<deleter_type>::type&& __d) noexcept
      : unique_ptr<_Tp, _Dp>(ptr) {}

  unique_ptr(unique_ptr&& u) noexcept {
    model_move(__cast_to_infer_ptr(this), __cast_to_infer_ptr(&u));
  }

  template <class _Up,
            class _Ep,
            typename = typename enable_if<
                !is_array<_Up>::value &&
                is_convertible<typename unique_ptr<_Up, _Ep>::pointer,
                               pointer>::value &&
                is_convertible<_Ep, deleter_type>::value &&
                (!is_reference<deleter_type>::value ||
                 is_same<deleter_type, _Ep>::value)>::type>
  unique_ptr(unique_ptr<_Up, _Ep>&& u) noexcept {
    model_move(__cast_to_infer_ptr(this), __cast_to_infer_ptr(&u));
  }

  template <
      class _Up,
      typename = typename enable_if<is_convertible<_Up*, _Tp*>::value>::type>
  unique_ptr(auto_ptr<_Up>&& __p) noexcept;

  ~unique_ptr() { reset(); }

  unique_ptr& operator=(unique_ptr&& __u) noexcept {
    model_move(__cast_to_infer_ptr(this), __cast_to_infer_ptr(&__u));
    return *this;
  }

  template <class _Up,
            class _Ep,
            typename = typename enable_if<
                !is_array<_Up>::value &&
                is_convertible<typename unique_ptr<_Up, _Ep>::pointer,
                               pointer>::value &&
                is_assignable<deleter_type&, _Ep&&>::value>::type>
  unique_ptr& operator=(unique_ptr<_Up, _Ep>&& __u) noexcept {
    model_move(__cast_to_infer_ptr(this), __cast_to_infer_ptr(&__u));
    return *this;
  }

  unique_ptr& operator=(nullptr_t) noexcept {
    reset();
    return *this;
  }
  typename add_lvalue_reference<_Tp>::type operator*() const
      INFER_MODEL_AS_DEREF_FIRST_ARG;

  pointer operator->() const INFER_MODEL_AS_DEREF_FIRST_ARG;

  pointer get() const INFER_MODEL_AS_DEREF_FIRST_ARG;

  typedef typename remove_reference<deleter_type>::type& _Dp_reference;
  typedef const typename remove_reference<deleter_type>::type&
      _Dp_const_reference;
  _Dp_const_reference get_deleter() const {}
  _Dp_reference get_deleter() {}

  explicit operator bool() const {
    return !!(bool)(*__cast_to_infer_ptr(this));
  }
  pointer release() INFER_MODEL_AS_DEREF_FIRST_ARG;

  void reset(pointer p = nullptr) { model_set(__cast_to_infer_ptr(this), p); }

  void swap(unique_ptr& u) {
    model_swap(__cast_to_infer_ptr(this), __cast_to_infer_ptr(&u));
  }
};

template <class _Tp, class _Dp>
struct unique_ptr<_Tp[], _Dp> {
  // use SFINAE to determine whether _Del::pointer exists
  class _Pointer {
    template <typename _Up>
    static typename _Up::pointer __test(typename _Up::pointer*);

    template <typename _Up>
    static _Tp* __test(...);

    typedef typename remove_reference<_Dp>::type _Del;

   public:
    typedef decltype(__test<_Del>(0)) type;
  };

 public:
  typedef typename _Pointer::type pointer;
  typedef _Tp element_type;
  typedef _Dp deleter_type;

 private:
  // translate shared_ptr as type pointer
  friend class infer_traits::TranslateAsType<pointer>;

  /// type of 'this' in unique_ptr<T> as seen by infer
  typedef const void** infer_unique_ptr_t;
// use it to avoid compilation errors and make infer analyzer happy
#define __cast_to_infer_ptr(self) ((infer_unique_ptr_t)self)

  static void model_set(infer_unique_ptr_t self, const void* value) noexcept {
    *self = value;
  }

  // in case 'pointer' type is not type pointer, enable catch all overload of
  // `model_set` to prevent compliation issues
  static void model_set(...) noexcept { /* no model for this overload */
  }

  static void model_move(infer_unique_ptr_t self,
                         infer_unique_ptr_t other) noexcept {
    *self = *other;
    model_set(other, nullptr);
  }

  static void model_swap(infer_unique_ptr_t infer_self,
                         infer_unique_ptr_t infer_other) noexcept {
    const void* t = *infer_self;
    *infer_self = *infer_other;
    *infer_other = t;
  }

  pointer __ignore__; // used to keep sizeof(unique_ptr) same as in standard

 public:
  constexpr unique_ptr() noexcept {
    model_set(__cast_to_infer_ptr(this), nullptr);
  }

  constexpr unique_ptr(nullptr_t) noexcept : unique_ptr() {}

  explicit unique_ptr(pointer ptr) noexcept {
    model_set(__cast_to_infer_ptr(this), ptr);
  }

  unique_ptr(
      pointer ptr,
      typename conditional<
          is_reference<deleter_type>::value,
          deleter_type,
                 typename add_lvalue_reference<const deleter_type>::type>::type
                 __d) noexcept
      : unique_ptr(ptr) {}

  unique_ptr(pointer ptr,
             typename remove_reference<deleter_type>::type&& __d) noexcept
      : unique_ptr(ptr) {}

  unique_ptr(unique_ptr&& u) noexcept {
    model_move(__cast_to_infer_ptr(this), __cast_to_infer_ptr(&u));
  }

  template <class _Up,
            class _Ep,
            typename = typename enable_if<
                is_array<_Up>::value &&
                is_convertible<typename unique_ptr<_Up, _Ep>::pointer,
                               pointer>::value &&
                is_convertible<_Ep, deleter_type>::value &&
                (!is_reference<deleter_type>::value ||
                 is_same<deleter_type, _Ep>::value)>::type>
  unique_ptr(unique_ptr<_Up, _Ep>&& u) noexcept {
    model_move(__cast_to_infer_ptr(this), __cast_to_infer_ptr(&u));
  }

  template <
      class _Up,
      typename = typename enable_if<is_convertible<_Up*, _Tp*>::value>::type>
  unique_ptr(auto_ptr<_Up>&& __p) noexcept;

  ~unique_ptr() { reset(); }

  unique_ptr& operator=(unique_ptr&& __u) noexcept {
    model_move(__cast_to_infer_ptr(this), __cast_to_infer_ptr(&__u));
    return *this;
  }

  template <class _Up,
            class _Ep,
            typename = typename enable_if<
                is_array<_Up>::value &&
                is_convertible<typename unique_ptr<_Up, _Ep>::pointer,
                               pointer>::value &&
                is_assignable<deleter_type&, _Ep&&>::value>::type>
  unique_ptr& operator=(unique_ptr<_Up, _Ep>&& __u) noexcept {
    model_move(__cast_to_infer_ptr(this), __cast_to_infer_ptr(&__u));
    return *this;
  }

  unique_ptr& operator=(nullptr_t) noexcept {
    reset();
    return *this;
  }

  typename add_lvalue_reference<_Tp>::type operator[](size_t i) const
      INFER_MODEL_AS_DEREF_FIRST_ARG;

  pointer get() const INFER_MODEL_AS_DEREF_FIRST_ARG;
  typedef typename remove_reference<deleter_type>::type& _Dp_reference;
  typedef const typename remove_reference<deleter_type>::type&
      _Dp_const_reference;
  _Dp_const_reference get_deleter() const {}
  _Dp_reference get_deleter() {}

  explicit operator bool() const {
    return !!(bool)(*__cast_to_infer_ptr(this));
  }
  pointer release() INFER_MODEL_AS_DEREF_FIRST_ARG;

  void reset(pointer p = nullptr) { model_set(__cast_to_infer_ptr(this), p); }

  void swap(unique_ptr& u) {
    model_swap(__cast_to_infer_ptr(this), __cast_to_infer_ptr(&u));
  }
};

template <class _T1, class _D1, class _T2, class _D2>
inline bool operator==(const unique_ptr<_T1, _D1>& __x,
                       const unique_ptr<_T2, _D2>& __y) {
  return __x.get() == __y.get();
}

template <class _T1, class _D1, class _T2, class _D2>
inline bool operator!=(const unique_ptr<_T1, _D1>& __x,
                       const unique_ptr<_T2, _D2>& __y) {
  return !(__x == __y);
}

template <class _T1, class _D1, class _T2, class _D2>
inline bool operator<(const unique_ptr<_T1, _D1>& __x,
                      const unique_ptr<_T2, _D2>& __y) {
  /*typedef typename unique_ptr<_T1, _D1>::pointer _P1;
  typedef typename unique_ptr<_T2, _D2>::pointer _P2;
  typedef typename common_type<_P1, _P2>::type _Vp;
  return less<_Vp>()(__x.get(), __y.get());*/
}

template <class _T1, class _D1, class _T2, class _D2>
inline bool operator>(const unique_ptr<_T1, _D1>& __x,
                      const unique_ptr<_T2, _D2>& __y) {
  return __y < __x;
}

template <class _T1, class _D1, class _T2, class _D2>
inline bool operator<=(const unique_ptr<_T1, _D1>& __x,
                       const unique_ptr<_T2, _D2>& __y) {
  return !(__y < __x);
}

template <class _T1, class _D1, class _T2, class _D2>
inline bool operator>=(const unique_ptr<_T1, _D1>& __x,
                       const unique_ptr<_T2, _D2>& __y) {
  return !(__x < __y);
}

template <class _T1, class _D1>
inline bool operator==(const unique_ptr<_T1, _D1>& __x, nullptr_t) {
  return !__x;
}

template <class _T1, class _D1>
inline bool operator==(nullptr_t, const unique_ptr<_T1, _D1>& __x) {
  return !__x;
}

template <class _T1, class _D1>
inline bool operator!=(const unique_ptr<_T1, _D1>& __x, nullptr_t) {
  return static_cast<bool>(__x);
}

template <class _T1, class _D1>
inline bool operator!=(nullptr_t, const unique_ptr<_T1, _D1>& __x) {
  return static_cast<bool>(__x);
}

template <class _T1, class _D1>
inline bool operator<(const unique_ptr<_T1, _D1>& __x, nullptr_t) {
  /*typedef typename unique_ptr<_T1, _D1>::pointer _P1;
  return less<_P1>()(__x.get(), nullptr);*/
}

template <class _T1, class _D1>
inline bool operator<(nullptr_t, const unique_ptr<_T1, _D1>& __x) {
  /*typedef typename unique_ptr<_T1, _D1>::pointer _P1;
  return less<_P1>()(nullptr, __x.get());*/
}

template <class _T1, class _D1>
inline bool operator>(const unique_ptr<_T1, _D1>& __x, nullptr_t) {
  return nullptr < __x;
}

template <class _T1, class _D1>
inline bool operator>(nullptr_t, const unique_ptr<_T1, _D1>& __x) {
  return __x < nullptr;
}

template <class _T1, class _D1>
inline bool operator<=(const unique_ptr<_T1, _D1>& __x, nullptr_t) {
  return !(nullptr < __x);
}

template <class _T1, class _D1>
inline bool operator<=(nullptr_t, const unique_ptr<_T1, _D1>& __x) {
  return !(__x < nullptr);
}

template <class _T1, class _D1>
inline bool operator>=(const unique_ptr<_T1, _D1>& __x, nullptr_t) {
  return !(__x < nullptr);
}

template <class _T1, class _D1>
inline bool operator>=(nullptr_t, const unique_ptr<_T1, _D1>& __x) {
  return !(nullptr < __x);
}

template <class T, class D>
struct hash<unique_ptr<T, D>> : public hash<std__unique_ptr<T, D>> {
  size_t operator()(const unique_ptr<T, D>& __u) const noexcept {}
};

template <typename _Tp>
struct _MakeUniq2 {
  typedef unique_ptr<_Tp> __single_object;
};

template <typename _Tp>
struct _MakeUniq2<_Tp[]> {
  typedef unique_ptr<_Tp[]> __array;
};

template <typename _Tp, size_t _Bound>
struct _MakeUniq2<_Tp[_Bound]> {
  struct __invalid_type {};
};

/// std::make_unique for single objects
template <typename _Tp, typename... _Args>
inline typename _MakeUniq2<_Tp>::__single_object make_unique(
    _Args&&... __args) {
  return unique_ptr<_Tp>(::new _Tp(std::forward<_Args>(__args)...));
}

/// std::make_unique for arrays of unknown bound
template <typename _Tp>
inline typename _MakeUniq2<_Tp>::__array make_unique(size_t __num) {
  return unique_ptr<_Tp>(::new typename remove_extent<_Tp>::type[__num]());
}

/// Disable std::make_unique for arrays of known bound
template <typename _Tp, typename... _Args>
inline typename _MakeUniq2<_Tp>::__invalid_type make_unique(_Args&&...) =
    delete;
INFER_NAMESPACE_STD_END

#undef __cast_to_infer_ptr
