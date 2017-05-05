/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#pragma once

#include <type_traits>
#include <climits>
#include <limits>
#include <initializer_list>
#include <stdexcept>
#include <memory>
#include <cstring>
#include <cassert>

#include <infer_model/common.h>
#include <infer_model/infer_traits.h>

#ifdef INFER_USE_LIBCPP
// libc++ vector header includes it, but it breaks
// compilation with stdlibc++ implementation
#include <algorithm>
#endif

INFER_NAMESPACE_STD_BEGIN

struct bool_ref {
  bool_ref& operator=(bool x) {}
  bool_ref& operator=(const bool_ref& x) {}

  operator bool() const noexcept {}
  void flip() noexcept {}

  // not part of C++ std, but required for model implementation to compile
  explicit bool_ref(bool x) {}
};

template <class T>
struct vector_ref {
  typedef T& ref;
};

template <>
struct vector_ref<bool> {
  typedef bool_ref ref;
};

// this function will be treated as SKIP by infer
template <class T>
T* __infer_skip__get_nondet_val() {}

template <class T>
void __infer_deref_first_arg(T* ptr) INFER_MODEL_AS_DEREF_FIRST_ARG;

// WARNING: do not add any new fields to std::vector model. sizeof(std::vector)
// = 24 !!
template <class _Tp, class _Allocator = allocator<_Tp>>
class vector {

 public:
  typedef vector __self;
  typedef _Tp value_type;
  typedef _Allocator allocator_type;
  typedef allocator_traits<allocator_type> __alloc_traits;
  typedef typename vector_ref<value_type>::ref reference;
  typedef const value_type& const_reference;
  typedef typename __alloc_traits::size_type size_type;
  typedef typename __alloc_traits::difference_type difference_type;
  typedef typename __alloc_traits::pointer pointer;
  typedef typename __alloc_traits::const_pointer const_pointer;

  typedef STD_ITER(pointer, vector) iterator;
  typedef STD_ITER(const_pointer, vector) const_iterator;
  typedef std::reverse_iterator<iterator> reverse_iterator;
  typedef std::reverse_iterator<const_iterator> const_reverse_iterator;

  /* INFER SPECIFIC HELPER FUNCTIONS */

  // required to keep sizeof(std::vector) same as in standard
  size_type infer_size = 0;
  void* _ignore1;
  void* _ignore2;

  void access_at(size_type index) {
    _Tp* dummy_array = (_Tp*) malloc(sizeof(value_type) * infer_size);
    auto dummy_value = dummy_array[index];
    free(dummy_array);
  }

  value_type* get() const {
    return __infer_skip__get_nondet_val<value_type>();
  }

  void allocate(size_type __n) {
    infer_size = __n;
  }

  void deallocate() {
    infer_size = 0;
  }

  template <class Iter>
  void allocate_iter(Iter begin, Iter end) {
    allocate(end - begin);
  }

  /* std::vector implementation */

  vector() noexcept(is_nothrow_default_constructible<allocator_type>::value) {
    deallocate();
  }

  explicit vector(const allocator_type& __a) noexcept { deallocate(); }

  explicit vector(size_type __n);
  // introduced in C++14
  explicit vector(size_type __n, const allocator_type& __a);
  vector(size_type __n, const_reference __x);
  vector(size_type __n, const_reference __x, const allocator_type& __a);
  template <class _ForwardIterator>
  vector(_ForwardIterator __first,
         typename enable_if<
             is_constructible<
                 value_type,
                 typename iterator_traits<_ForwardIterator>::reference>::value,
             _ForwardIterator>::type __last);
  template <class _ForwardIterator>
  vector(_ForwardIterator __first,
         _ForwardIterator __last,
         const allocator_type& __a,
         typename enable_if<is_constructible<
             value_type,
             typename iterator_traits<_ForwardIterator>::reference>::value>::
             type* = 0);

  vector(initializer_list<value_type> __il);

  vector(initializer_list<value_type> __il, const allocator_type& __a);

  vector(const vector& __x);
  vector(const vector& __x, const allocator_type& __a);

  vector& operator=(const vector& __x);

  vector(vector&& __x) noexcept;

  vector(vector&& __x, const allocator_type& __a);

  vector& operator=(vector&& __x) noexcept;
  /*((__noexcept_move_assign_container<_Allocator, __alloc_traits>::value)); */

  vector& operator=(initializer_list<value_type> __il) {
    assign(__il.begin(), __il.end());
    return *this;
  }

  template <class _ForwardIterator>
  typename enable_if<is_constructible<value_type,
                                      typename iterator_traits<
                                          _ForwardIterator>::reference>::value,
                     void>::type
  assign(_ForwardIterator __first, _ForwardIterator __last);

  void assign(size_type __n, const_reference __u);

  void assign(initializer_list<value_type> __il) {
    assign(__il.begin(), __il.end());
  }

  allocator_type get_allocator() const {}

  iterator begin() noexcept;
  const_iterator begin() const noexcept;
  iterator end() noexcept;
  const_iterator end() const noexcept;

  reverse_iterator rbegin() noexcept { return reverse_iterator(end()); }

  const_reverse_iterator rbegin() const noexcept {
    return const_reverse_iterator(end());
  }

  reverse_iterator rend() noexcept { return reverse_iterator(begin()); }

  const_reverse_iterator rend() const noexcept {
    return const_reverse_iterator(begin());
  }

  const_iterator cbegin() const noexcept { return begin(); }

  const_iterator cend() const noexcept { return end(); }

  const_reverse_iterator crbegin() const noexcept { return rbegin(); }

  const_reverse_iterator crend() const noexcept { return rend(); }

  size_type size() const noexcept {
    return infer_size;
  }

  size_type capacity() const noexcept {}

  bool empty() const noexcept { return infer_size == 0; }
  size_type max_size() const noexcept;
  void reserve(size_type __n);
  void shrink_to_fit() noexcept;

  reference operator[](size_type __n);
  const_reference operator[](size_type __n) const;
  reference at(size_type __n);
  const_reference at(size_type __n) const;

  reference front() { return (reference)*get(); }
  const_reference front() const { return (const_reference)*get(); }
  reference back() { return (reference)*get(); }
  const_reference back() const { return (const_reference)*get(); }

  value_type* data() noexcept { return get(); }

  const value_type* data() const noexcept { return get(); }

  void push_back(const_reference __x);
  void push_back(value_type&& __x);
  template <class... _Args>

  void emplace_back(_Args&&... __args);

  void pop_back();

  iterator insert(const_iterator __position, const_reference __x);
  iterator insert(const_iterator __position, value_type&& __x);
  template <class... _Args>
  iterator emplace(const_iterator __position, _Args&&... __args);
  iterator insert(const_iterator __position,
                  size_type __n,
                  const_reference __x);
  template <class _ForwardIterator>
  typename enable_if<is_constructible<value_type,
                                      typename iterator_traits<
                                          _ForwardIterator>::reference>::value,
                     iterator>::type
  insert(const_iterator __position,
         _ForwardIterator __first,
         _ForwardIterator __last);

  iterator insert(const_iterator __position,
                  initializer_list<value_type> __il) {
    return insert(__position, __il.begin(), __il.end());
  }

  iterator erase(const_iterator __position);
  iterator erase(const_iterator __first, const_iterator __last);

  void clear() noexcept { deallocate(); }

  void resize(size_type __sz);
  void resize(size_type __sz, const_reference __x);

  // NOTE c++17 adds noexcept
  void swap(vector&);

 private:
  iterator __make_iter(pointer __p) noexcept;

  const_iterator __make_iter(const_pointer __p) const noexcept;
};

template <class _Tp, class _Allocator>
typename vector<_Tp, _Allocator>::size_type vector<_Tp, _Allocator>::max_size()
    const noexcept {
}

template <class _Tp, class _Allocator>
vector<_Tp, _Allocator>::vector(size_type __n) {
  allocate(__n);
}

template <class _Tp, class _Allocator>
vector<_Tp, _Allocator>::vector(size_type __n, const allocator_type& __a) {
  allocate(__n);
}

template <class _Tp, class _Allocator>
vector<_Tp, _Allocator>::vector(size_type __n, const_reference __x) {
  allocate(__n);
}

template <class _Tp, class _Allocator>
vector<_Tp, _Allocator>::vector(size_type __n,
                                const_reference __x,
                                const allocator_type& __a) {
  allocate(__n);
}

template <class _Tp, class _Allocator>
template <class _ForwardIterator>
vector<_Tp, _Allocator>::vector(
    _ForwardIterator __first,
    typename enable_if<
        is_constructible<
            value_type,
            typename iterator_traits<_ForwardIterator>::reference>::value,
        _ForwardIterator>::type __last) {
  allocate_iter(__first, __last);
}

template <class _Tp, class _Allocator>
template <class _ForwardIterator>
vector<_Tp, _Allocator>::vector(
    _ForwardIterator __first,
    _ForwardIterator __last,
    const allocator_type& __a,
    typename enable_if<is_constructible<
        value_type,
        typename iterator_traits<_ForwardIterator>::reference>::value>::type*) {
  allocate_iter(__first, __last);
}

template <class _Tp, class _Allocator>
vector<_Tp, _Allocator>::vector(const vector& __x) {
  allocate(__x.size());
}

template <class _Tp, class _Allocator>
vector<_Tp, _Allocator>::vector(const vector& __x, const allocator_type& __a) {
  allocate(__x.size());
}

template <class _Tp, class _Allocator>
inline vector<_Tp, _Allocator>::vector(vector&& __x) noexcept {
  allocate(__x.size());
  __x.clear();
}

template <class _Tp, class _Allocator>
inline vector<_Tp, _Allocator>::vector(vector&& __x,
                                       const allocator_type& __a) {
  allocate(__x.size());
  __x.clear();
}

template <class _Tp, class _Allocator>
inline vector<_Tp, _Allocator>::vector(initializer_list<value_type> __il) {
  allocate_iter(__il.begin(), __il.end());
}

template <class _Tp, class _Allocator>
inline vector<_Tp, _Allocator>::vector(initializer_list<value_type> __il,
                                       const allocator_type& __a) {
  allocate_iter(__il.begin(), __il.end());
}

template <class _Tp, class _Allocator>
inline vector<_Tp, _Allocator>& vector<_Tp, _Allocator>::operator=(
    vector&& __x) noexcept /*((__noexcept_move_assign_container<_Allocator,
                              __alloc_traits>::value)) */
{
  allocate(__x.size());
  __x.clear();
  return *this;
}

template <class _Tp, class _Allocator>
inline vector<_Tp, _Allocator>& vector<_Tp, _Allocator>::operator=(
    const vector& __x) {
  allocate(__x.size());
  return *this;
}

template <class _Tp, class _Allocator>
template <class _ForwardIterator>
typename enable_if<is_constructible<_Tp,
                                    typename iterator_traits<
                                        _ForwardIterator>::reference>::value,
                   void>::type
vector<_Tp, _Allocator>::assign(_ForwardIterator __first,
                                _ForwardIterator __last) {
  allocate_iter(__first, __last);
}

template <class _Tp, class _Allocator>
void vector<_Tp, _Allocator>::assign(size_type __n, const_reference __u) {
  allocate(__n);
}

template <class _Tp, class _Allocator>
inline typename vector<_Tp, _Allocator>::iterator
vector<_Tp, _Allocator>::__make_iter(pointer __p) noexcept {
  return iterator(__p);
}

template <class _Tp, class _Allocator>
inline typename vector<_Tp, _Allocator>::const_iterator
vector<_Tp, _Allocator>::__make_iter(const_pointer __p) const noexcept {
  return const_iterator(__p);
}

template <class _Tp, class _Allocator>
inline typename vector<_Tp, _Allocator>::iterator
vector<_Tp, _Allocator>::begin() noexcept {
  return __make_iter(get());
}

template <class _Tp, class _Allocator>
inline typename vector<_Tp, _Allocator>::const_iterator
vector<_Tp, _Allocator>::begin() const noexcept {
  return __make_iter(get());
}

template <class _Tp, class _Allocator>
inline typename vector<_Tp, _Allocator>::iterator
vector<_Tp, _Allocator>::end() noexcept {
  return __make_iter(get());
}

template <class _Tp, class _Allocator>
inline typename vector<_Tp, _Allocator>::const_iterator
vector<_Tp, _Allocator>::end() const noexcept {
  return __make_iter(get());
}

template <class _Tp, class _Allocator>
inline typename vector<_Tp, _Allocator>::reference vector<_Tp, _Allocator>::
operator[](size_type __n) {
  access_at(__n);
  return (reference)*get();
}

template <class _Tp, class _Allocator>
inline typename vector<_Tp, _Allocator>::const_reference
    vector<_Tp, _Allocator>::operator[](size_type __n) const {
  return (const_reference)*get();
}

template <class _Tp, class _Allocator>
typename vector<_Tp, _Allocator>::reference vector<_Tp, _Allocator>::at(
    size_type __n) {
  access_at(__n);
  return (reference)*get();
}

template <class _Tp, class _Allocator>
typename vector<_Tp, _Allocator>::const_reference vector<_Tp, _Allocator>::at(
    size_type __n) const {
  access_at(__n);
  return (const_reference)*get();
}

template <class _Tp, class _Allocator>
void vector<_Tp, _Allocator>::reserve(size_type __n) {}

template <class _Tp, class _Allocator>
void vector<_Tp, _Allocator>::shrink_to_fit() noexcept {}

template <class _Tp, class _Allocator>
inline void vector<_Tp, _Allocator>::push_back(const_reference __x) {
  infer_size++;
}

template <class _Tp, class _Allocator>
inline void vector<_Tp, _Allocator>::push_back(value_type&& __x) {
  infer_size++;
}

template <class _Tp, class _Allocator>
template <class... _Args>
inline void vector<_Tp, _Allocator>::emplace_back(_Args&&... __args) {
  infer_size++;
}

template <class _Tp, class _Allocator>
inline void vector<_Tp, _Allocator>::pop_back() {
  infer_size--;
}

template <class _Tp, class _Allocator>
inline typename vector<_Tp, _Allocator>::iterator
vector<_Tp, _Allocator>::erase(const_iterator __position) {
  infer_size--;
  return __position;
}

template <class _Tp, class _Allocator>
typename vector<_Tp, _Allocator>::iterator vector<_Tp, _Allocator>::erase(
    const_iterator __first, const_iterator __last) {
  infer_size -= __last - __first;
  return __first;
}

template <class _Tp, class _Allocator>
typename vector<_Tp, _Allocator>::iterator vector<_Tp, _Allocator>::insert(
    const_iterator __position, const_reference __x) {
  infer_size++;
}

template <class _Tp, class _Allocator>
typename vector<_Tp, _Allocator>::iterator vector<_Tp, _Allocator>::insert(
    const_iterator __position, value_type&& __x) {
  infer_size++;
}

template <class _Tp, class _Allocator>
template <class... _Args>
typename vector<_Tp, _Allocator>::iterator vector<_Tp, _Allocator>::emplace(
    const_iterator __position, _Args&&... __args) {
  infer_size++;
}

template <class _Tp, class _Allocator>
typename vector<_Tp, _Allocator>::iterator vector<_Tp, _Allocator>::insert(
    const_iterator __position, size_type __n, const_reference __x) {
  infer_size += __n;
}

template <class _Tp, class _Allocator>
template <class _ForwardIterator>
typename enable_if<is_constructible<_Tp,
                                    typename iterator_traits<
                                        _ForwardIterator>::reference>::value,
                   typename vector<_Tp, _Allocator>::iterator>::type
vector<_Tp, _Allocator>::insert(const_iterator __position,
                                _ForwardIterator __first,
                                _ForwardIterator __last) {
  infer_size += __last - __first;
}

template <class _Tp, class _Allocator>
void vector<_Tp, _Allocator>::resize(size_type __sz) {
  infer_size = __sz;
}

template <class _Tp, class _Allocator>
void vector<_Tp, _Allocator>::resize(size_type __sz, const_reference __x) {
  infer_size = __sz;
}

template <class _Tp, class _Allocator>
void vector<_Tp, _Allocator>::swap(vector& __x) {
  auto tmp = __x.size();
  __x.resize(infer_size);
  infer_size = tmp;
}

template <class _Allocator>
struct hash<vector<bool, _Allocator>>
    : public unary_function<vector<bool, _Allocator>, size_t> {

  size_t operator()(const vector<bool, _Allocator>& __vec) const noexcept {}
};

template <class _Tp, class _Allocator>
inline bool operator==(const vector<_Tp, _Allocator>& __x,
                       const vector<_Tp, _Allocator>& __y) {
  const typename vector<_Tp, _Allocator>::size_type __sz = __x.size();
  return __sz == __y.size() && equal(__x.begin(), __x.end(), __y.begin());
}

template <class _Tp, class _Allocator>
inline bool operator!=(const vector<_Tp, _Allocator>& __x,
                       const vector<_Tp, _Allocator>& __y) {
  return !(__x == __y);
}

template <class _Tp, class _Allocator>
inline bool operator<(const vector<_Tp, _Allocator>& __x,
                      const vector<_Tp, _Allocator>& __y) {
  return lexicographical_compare(
      __x.begin(), __x.end(), __y.begin(), __y.end());
}

template <class _Tp, class _Allocator>
inline bool operator>(const vector<_Tp, _Allocator>& __x,
                      const vector<_Tp, _Allocator>& __y) {
  return __y < __x;
}

template <class _Tp, class _Allocator>
inline bool operator>=(const vector<_Tp, _Allocator>& __x,
                       const vector<_Tp, _Allocator>& __y) {
  return !(__x < __y);
}

template <class _Tp, class _Allocator>
inline bool operator<=(const vector<_Tp, _Allocator>& __x,
                       const vector<_Tp, _Allocator>& __y) {
  return !(__y < __x);
}

template <class _Tp, class _Allocator>
inline void swap(vector<_Tp, _Allocator>& __x, vector<_Tp, _Allocator>& __y) {
  __x.swap(__y);
}

INFER_NAMESPACE_STD_END
