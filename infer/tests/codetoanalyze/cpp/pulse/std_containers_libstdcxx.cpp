/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// Mirrors how libstdc++ declares std::set and std::list, whose iterator
// classes are not modelled. Do not include C++ standard headers.
namespace std {

template <typename _Tp>
struct _Rb_tree_const_iterator {
  _Rb_tree_const_iterator& operator++();
  const _Tp& operator*() const;
  void* _M_node;
};

template <typename _Tp>
bool operator!=(const _Rb_tree_const_iterator<_Tp>& __x,
                const _Rb_tree_const_iterator<_Tp>& __y);

template <typename _Key>
struct set {
  typedef _Rb_tree_const_iterator<_Key> iterator;
  set(const set& __x);
  ~set();
  iterator begin() const;
  iterator end() const;
  void* _M_impl;
};

template <typename _Tp>
struct list {
  ~list();
  _Tp& front();
  void clear();
  void* _M_impl;
};

} // namespace std

int iterate_local_set_bad(const std::set<int>& set) {
  std::set<int> local = set;
  int sum = 0;
  for (std::set<int>::iterator it = local.begin(); it != local.end(); ++it) {
    sum += *it;
  }
  return sum;
}

int list_front_after_clear_bad(std::list<int>& list) {
  int& first = list.front();
  list.clear();
  return first;
}
