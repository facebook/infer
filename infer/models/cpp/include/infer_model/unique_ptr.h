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

// define unique_ptr outside of std namespace. It will be aliased as
// std::unique_ptr via 'using' clause later
namespace infer_std_model {

// IMPORTANT
// There is specialization of unique_ptr below and it's mosly copy paste.
// When changing model, remember to change it for specialization as well!
template <class _Tp, class _Dp>
struct unique_ptr {
  // use SFINAE to determine whether _Del::pointer exists
  class _Pointer {
    template <typename _Up>
    static typename _Up::pointer __test(typename _Up::pointer*);

    template <typename _Up>
    static _Tp* __test(...);

    typedef typename std::remove_reference<_Dp>::type _Del;

   public:
    typedef decltype(__test<_Del>(0)) type;
  };

 public:
  typedef typename _Pointer::type pointer;
  typedef _Tp element_type;
  typedef _Dp deleter_type;

  pointer data;
  template <class Y>
  unique_ptr(const std::std__unique_ptr<Y>& u) {}

  constexpr unique_ptr() noexcept : data(nullptr) {}

  constexpr unique_ptr(std::nullptr_t) noexcept : unique_ptr<_Tp, _Dp>() {}

  explicit unique_ptr(pointer ptr) : data(ptr) {}

  unique_ptr(pointer ptr,
      typename std::conditional<
          std::is_reference<deleter_type>::value,
          deleter_type,
          typename std::add_lvalue_reference<const deleter_type>::type>::type
          __d) noexcept : data(ptr) {}

  unique_ptr(pointer ptr,
             typename std::remove_reference<deleter_type>::type&& __d) noexcept
      : data(ptr) {}

  unique_ptr(unique_ptr&& u) noexcept : data(u.data) { u.data = nullptr; }

  template <class _Up,
            class _Ep,
            typename = typename std::enable_if<
                !std::is_array<_Up>::value &&
                std::is_convertible<typename unique_ptr<_Up, _Ep>::pointer,
                                    pointer>::value &&
                std::is_convertible<_Ep, deleter_type>::value &&
                (!std::is_reference<deleter_type>::value ||
                 std::is_same<deleter_type, _Ep>::value)>::type>
  unique_ptr(unique_ptr<_Up, _Ep>&& u) noexcept : data(u.data) {
    u.data = nullptr;
  }

  template <
      class _Up,
            typename = typename std::enable_if<
                std::is_convertible<_Up*, _Tp*>::value>::type>
  unique_ptr(std::auto_ptr<_Up>&& __p) noexcept;

  ~unique_ptr() { reset(); }

  unique_ptr& operator=(unique_ptr&& __u) noexcept {
    reset(__u.data);
    return *this;
  }

  template <class _Up,
            class _Ep,
            typename = typename std::enable_if<
                !std::is_array<_Up>::value &&
                std::is_convertible<typename unique_ptr<_Up, _Ep>::pointer,
                                    pointer>::value &&
                std::is_assignable<deleter_type&, _Ep&&>::value>::type>
  unique_ptr& operator=(unique_ptr<_Up, _Ep>&& __u) noexcept {
    reset(__u.data);
    return *this;
  }

  unique_ptr& operator=(std::nullptr_t) noexcept {
    reset();
    return *this;
  }
  typename std::add_lvalue_reference<_Tp>::type operator*() const {
    return *data;
  }

  pointer operator->() const { return data; }

  pointer get() const { return data; }

  typedef typename std::remove_reference<deleter_type>::type& _Dp_reference;
  typedef const typename std::remove_reference<deleter_type>::type&
      _Dp_const_reference;
  _Dp_const_reference get_deleter() const {}
  _Dp_reference get_deleter() {}

  explicit operator bool() const { return data != nullptr; }
  pointer release() { return data; }

  void reset(pointer p = nullptr) { data = p; }

  void swap(unique_ptr& u) {
    pointer tmp = data;
    data = u.data;
    u.data = tmp;
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

    typedef typename std::remove_reference<_Dp>::type _Del;

   public:
    typedef decltype(__test<_Del>(0)) type;
  };

 public:
  typedef typename _Pointer::type pointer;
  typedef _Tp element_type;
  typedef _Dp deleter_type;

  pointer data;

  constexpr unique_ptr() noexcept : data(nullptr) {}

  constexpr unique_ptr(std::nullptr_t) noexcept : data(nullptr) {}

  explicit unique_ptr(pointer ptr) : data(ptr) {}

  unique_ptr(
      pointer ptr,
             typename std::conditional<std::is_reference<deleter_type>::value,
                                       deleter_type,
                                       typename std::add_lvalue_reference<
                                           const deleter_type>::type>::type __d)
      : data(ptr) {}

  unique_ptr(pointer ptr,
             typename std::remove_reference<deleter_type>::type&& __d)
      : data(ptr) {}

  unique_ptr(unique_ptr&& u) : data(u.data) { u.data = nullptr; }

  template <class _Up,
            class _Ep,
            typename = typename std::enable_if<
                std::is_array<_Up>::value &&
                std::is_convertible<typename unique_ptr<_Up, _Ep>::pointer,
                                    pointer>::value &&
                std::is_convertible<_Ep, deleter_type>::value &&
                (!std::is_reference<deleter_type>::value ||
                 std::is_same<deleter_type, _Ep>::value)>::type>
  unique_ptr(unique_ptr<_Up, _Ep>&& u) : data(u.data) {
    u.data = nullptr;
  }

  template <
      class _Up,
            typename = typename std::enable_if<
                std::is_convertible<_Up*, _Tp*>::value>::type>
  unique_ptr(std::auto_ptr<_Up>&& __p) noexcept;

  ~unique_ptr() { reset(); }

  unique_ptr& operator=(unique_ptr&& __u) {
    reset(__u.data);
    return *this;
  }

  template <class _Up,
            class _Ep,
            typename = typename std::enable_if<
                std::is_array<_Up>::value &&
                std::is_convertible<typename unique_ptr<_Up, _Ep>::pointer,
                                    pointer>::value &&
                std::is_assignable<deleter_type&, _Ep&&>::value>::type>
  unique_ptr& operator=(unique_ptr<_Up, _Ep>&& __u) {
    reset(__u.data);
    return *this;
  }

  unique_ptr& operator=(std::nullptr_t) {
    reset();
    return *this;
  }

  typename std::add_lvalue_reference<_Tp>::type operator[](size_t i) const {}

  pointer get() const { return data; }

  typedef typename std::remove_reference<deleter_type>::type& _Dp_reference;
  typedef const typename std::remove_reference<deleter_type>::type&
      _Dp_const_reference;
  _Dp_const_reference get_deleter() const {}
  _Dp_reference get_deleter() {}

  explicit operator bool() const { return data != nullptr; }
  pointer release() { return data; }

  void reset(pointer p = nullptr) { data = p; }

  void swap(unique_ptr& u) {
    pointer tmp = data;
    data = u.data;
    u.data = tmp;
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
inline bool operator==(const unique_ptr<_T1, _D1>& __x, std::nullptr_t) {
  return !__x;
}

template <class _T1, class _D1>
inline bool operator==(std::nullptr_t, const unique_ptr<_T1, _D1>& __x) {
  return !__x;
}

template <class _T1, class _D1>
inline bool operator!=(const unique_ptr<_T1, _D1>& __x, std::nullptr_t) {
  return static_cast<bool>(__x);
}

template <class _T1, class _D1>
inline bool operator!=(std::nullptr_t, const unique_ptr<_T1, _D1>& __x) {
  return static_cast<bool>(__x);
}

template <class _T1, class _D1>
inline bool operator<(const unique_ptr<_T1, _D1>& __x, std::nullptr_t) {
  /*typedef typename unique_ptr<_T1, _D1>::pointer _P1;
  return less<_P1>()(__x.get(), nullptr);*/
}

template <class _T1, class _D1>
inline bool operator<(std::nullptr_t, const unique_ptr<_T1, _D1>& __x) {
  /*typedef typename unique_ptr<_T1, _D1>::pointer _P1;
  return less<_P1>()(nullptr, __x.get());*/
}

template <class _T1, class _D1>
inline bool operator>(const unique_ptr<_T1, _D1>& __x, std::nullptr_t) {
  return nullptr < __x;
}

template <class _T1, class _D1>
inline bool operator>(std::nullptr_t, const unique_ptr<_T1, _D1>& __x) {
  return __x < nullptr;
}

template <class _T1, class _D1>
inline bool operator<=(const unique_ptr<_T1, _D1>& __x, std::nullptr_t) {
  return !(nullptr < __x);
}

template <class _T1, class _D1>
inline bool operator<=(std::nullptr_t, const unique_ptr<_T1, _D1>& __x) {
  return !(__x < nullptr);
}

template <class _T1, class _D1>
inline bool operator>=(const unique_ptr<_T1, _D1>& __x, std::nullptr_t) {
  return !(__x < nullptr);
}

template <class _T1, class _D1>
inline bool operator>=(std::nullptr_t, const unique_ptr<_T1, _D1>& __x) {
  return !(nullptr < __x);
}
} // namespace infer_std_model

INFER_NAMESPACE_STD_BEGIN

// make std::unique_ptr alias of infer_std_model::unique_ptr
template <class _Tp, class _Dp = default_delete<_Tp>>
using unique_ptr = infer_std_model::unique_ptr<_Tp, _Dp>;


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
  return unique_ptr<_Tp>(new _Tp(std::forward<_Args>(__args)...));
}

/// std::make_unique for arrays of unknown bound
template <typename _Tp>
inline typename _MakeUniq2<_Tp>::__array make_unique(size_t __num) {
  return unique_ptr<_Tp>(new typename remove_extent<_Tp>::type[__num]());
}

/// Disable std::make_unique for arrays of known bound
template <typename _Tp, typename... _Args>
inline typename _MakeUniq2<_Tp>::__invalid_type make_unique(_Args&&...) =
    delete;
INFER_NAMESPACE_STD_END
