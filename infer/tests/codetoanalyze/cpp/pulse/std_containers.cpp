/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <algorithm>
#include <deque>
#include <iterator>
#include <list>
#include <map>
#include <set>
#include <unordered_map>
#include <unordered_set>

int map_find_empty_bad() {
  std::map<int, int> map;
  auto it = map.find(1);
  return it->second;
}

int unordered_map_find_empty_bad() {
  std::unordered_map<int, int> map;
  auto it = map.find(1);
  return it->second;
}

int map_find_after_clear_bad(std::map<int, int>& map) {
  map.clear();
  return map.find(1)->second;
}

int set_begin_empty_bad() {
  std::set<int> set;
  return *set.begin();
}

int unordered_set_find_empty_bad() {
  std::unordered_set<int> set;
  return *set.find(1);
}

int list_empty_begin_bad(std::list<int>& list) {
  if (list.empty()) {
    return *list.begin();
  }
  return 0;
}

int map_find_compare_end_bad(std::map<int, int>& map) {
  auto it = map.find(1);
  if (it == map.end()) {
    return it->second;
  }
  return 0;
}

struct Containers {
  std::list<int> list;
  std::map<int, int> map;
  std::deque<int> deque;
  const std::list<int>& get_list() const { return list; }
};

int range_for_member_list_then_npe_bad(Containers& containers) {
  for (int x : containers.list) {
    (void)x;
  }
  int* p = nullptr;
  return *p;
}

int range_for_member_map_then_npe_bad(Containers& containers) {
  for (const auto& entry : containers.map) {
    (void)entry;
  }
  int* p = nullptr;
  return *p;
}

int range_for_member_deque_then_npe_bad(Containers& containers) {
  for (int x : containers.deque) {
    (void)x;
  }
  int* p = nullptr;
  return *p;
}

int range_for_accessor_then_npe_bad(const Containers& containers) {
  for (int x : containers.get_list()) {
    (void)x;
  }
  int* p = nullptr;
  return *p;
}

int set_iterator_loop_then_npe_bad(std::set<int>& set) {
  for (auto it = set.begin(); it != set.end(); ++it) {
    if (*it == 0) {
      break;
    }
  }
  int* p = nullptr;
  return *p;
}

int lookup(const std::map<int, int>& map, int key) { return map.at(key); }

int call_at_then_npe_bad(std::map<int, int>& map) {
  int value = lookup(map, 1);
  int* p = nullptr;
  return *p + value;
}

int first_value(std::list<int>& list) { return list.front(); }

int call_front_then_npe_bad(std::list<int>& list) {
  int value = first_value(list);
  int* p = nullptr;
  return *p + value;
}

int last_value(std::deque<int>& deque) { return deque.back(); }

int call_back_then_use_after_delete_bad(std::deque<int>& deque, int* p) {
  delete p;
  int value = last_value(deque);
  return *p + value;
}

int& first_reference(std::list<int>& list) { return list.front(); }

int call_front_then_clear_reference_bad(std::list<int>& list) {
  int& front = first_reference(list);
  list.clear();
  return front;
}

int at_then_empty_ok(std::map<int, int>& map) {
  int value = map.at(1);
  if (map.empty()) {
    int* p = nullptr;
    return *p;
  }
  return value;
}

int call_at_then_empty_ok(std::map<int, int>& map) {
  int value = lookup(map, 1);
  if (map.empty()) {
    int* p = nullptr;
    return *p;
  }
  return value;
}

int map_end_bad(std::map<int, int>& map) { return map.end()->second; }

int list_end_bad(std::list<int>& list) { return *list.end(); }

void map_erase_end_bad(std::map<int, int>& map) { map.erase(map.end()); }

int list_erase_bad(std::list<int>& list) {
  auto it = list.begin();
  list.erase(it);
  return *it;
}

int set_erase_bad(std::set<int>& set) {
  auto it = set.find(1);
  set.erase(it);
  return *it;
}

int multiset_erase_bad(std::multiset<int>& set) {
  auto it = set.begin();
  set.erase(it);
  return *it;
}

int unordered_set_erase_bad(std::unordered_set<int>& set) {
  auto it = set.begin();
  set.erase(it);
  return *it;
}

int map_erase_copy_bad(std::map<int, int>& map) {
  auto it = map.begin();
  auto it2 = it;
  map.erase(it);
  return it2->second;
}

void map_erase_in_loop_bad(std::map<int, int>& map) {
  for (auto it = map.begin(); it != map.end(); ++it) {
    if (it->second == 0) {
      map.erase(it);
    }
  }
}

void unordered_map_erase_in_loop_bad(std::unordered_map<int, int>& map) {
  for (auto it = map.begin(); it != map.end(); ++it) {
    if (it->second == 0) {
      map.erase(it);
    }
  }
}

int map_erase_reference_bad(std::map<int, int>& map) {
  auto it = map.find(1);
  const int& value = it->second;
  map.erase(it);
  return value;
}

int map_clear_reference_bad(std::map<int, int>& map) {
  const int& value = map[1];
  map.clear();
  return value;
}

int list_clear_iterator_bad(std::list<int>& list) {
  auto it = list.begin();
  list.clear();
  return *it;
}

int list_clear_reference_bad(std::list<int>& list) {
  const int& front = list.front();
  list.clear();
  return front;
}

int deref_list_iterator(std::list<int>::iterator it) { return *it; }

int call_deref_after_erase_bad(std::list<int>& list) {
  auto it = list.begin();
  list.erase(it);
  return deref_list_iterator(it);
}

void clear_list(std::list<int>& list) { list.clear(); }

int call_clear_after_begin_bad(std::list<int>& list) {
  auto it = list.begin();
  clear_list(list);
  return *it;
}

int deque_push_back_bad(std::deque<int>& deque) {
  auto it = deque.begin();
  deque.push_back(1);
  return *it;
}

int deque_push_front_bad(std::deque<int>& deque) {
  auto it = deque.end();
  deque.push_front(1);
  --it;
  return *it;
}

int deque_insert_bad(std::deque<int>& deque) {
  auto it = deque.begin();
  deque.insert(deque.end(), 1);
  return *it;
}

int map_find_checked_ok(std::map<int, int>& map) {
  auto it = map.find(1);
  if (it != map.end()) {
    return it->second;
  }
  return 0;
}

// whether the key is present is unknown so the lookup is not reported
int map_find_unchecked_ok(std::map<int, int>& map) {
  return map.find(1)->second;
}

int map_find_after_insert_ok() {
  std::map<int, int> map;
  map[1] = 2;
  return map.find(1)->second;
}

int unordered_map_find_after_emplace_ok() {
  std::unordered_map<int, int> map;
  map.emplace(1, 2);
  return map.find(1)->second;
}

void map_of_sets_find_after_emplace_ok(std::map<int, std::set<int>>& map) {
  map.emplace(1, std::set<int>());
  auto it = map.find(1);
  it->second.emplace(2);
}

int set_conditional_insert_empty_ok(bool b) {
  std::set<int> set;
  if (b) {
    set.insert(1);
  }
  if (!set.empty()) {
    return *set.begin();
  }
  return 0;
}

int map_conditional_insert_count_ok(bool b) {
  std::map<int, int> map;
  if (b) {
    map.emplace(1, 2);
  }
  if (map.count(1) > 0) {
    return map.find(1)->second;
  }
  return 0;
}

int list_conditional_insert_size_ok(bool b) {
  std::list<int> list;
  if (b) {
    list.push_back(1);
  }
  if (list.size() > 0) {
    return *list.begin();
  }
  return 0;
}

void insert_one(std::map<int, int>& map) { map.emplace(1, 2); }

int map_find_after_callee_insert_ok() {
  std::map<int, int> map;
  insert_one(map);
  return map.find(1)->second;
}

int list_copy_back_inserter_ok() {
  int a[] = {1, 2};
  std::list<int> list;
  std::copy(a, a + 2, std::back_inserter(list));
  return *list.begin();
}

int deque_copy_front_inserter_ok() {
  int a[] = {1, 2};
  std::deque<int> deque;
  std::copy(a, a + 2, std::front_inserter(deque));
  return *deque.begin();
}

int set_copy_inserter_ok() {
  int a[] = {1, 2};
  std::set<int> set;
  std::copy(a, a + 2, std::inserter(set, set.end()));
  return *set.begin();
}

int map_copy_inserter_find_ok() {
  std::pair<int, int> a[] = {{1, 2}};
  std::map<int, int> map;
  std::copy(a, a + 1, std::inserter(map, map.end()));
  return map.find(1)->second;
}

// references to the elements of node-based containers stay valid across
// insertions, including rehashing
int unordered_map_reference_stability_ok(std::unordered_map<int, int>& map) {
  const int& value = map[1];
  map[2] = 3;
  map.emplace(4, 5);
  return value;
}

int list_insert_iterator_ok(std::list<int>& list) {
  auto it = list.begin();
  list.push_back(1);
  list.push_front(2);
  return *it;
}

void map_erase_loop_ok(std::map<int, int>& map) {
  for (auto it = map.begin(); it != map.end();) {
    if (it->second == 0) {
      it = map.erase(it);
    } else {
      ++it;
    }
  }
}

void set_erase_postfix_loop_ok(std::set<int>& set) {
  for (auto it = set.begin(); it != set.end();) {
    if (*it == 0) {
      set.erase(it++);
    } else {
      ++it;
    }
  }
}

int map_iterate_ok(const std::map<int, int>& map) {
  int sum = 0;
  for (const auto& entry : map) {
    sum += entry.second;
  }
  return sum;
}

int empty_map_iterate_ok() {
  std::map<int, int> map;
  int sum = 0;
  for (const auto& entry : map) {
    sum += entry.second;
  }
  return sum;
}

int list_end_decrement_ok(std::list<int>& list) {
  auto it = list.end();
  --it;
  return *it;
}

// insertions at either end of a deque keep references valid
int deque_push_back_reference_ok(std::deque<int>& deque) {
  const int& front = deque.front();
  deque.push_back(1);
  return front;
}

int deque_iterator_after_push_back_ok(std::deque<int>& deque) {
  deque.push_back(1);
  auto it = deque.begin();
  return *it;
}

// operator< is translated and reads the fields of the iterators, which the
// models do not write
bool deque_iterator_compare_ok(std::deque<int>& deque) {
  auto first = deque.begin();
  auto last = deque.end();
  return first < last;
}

// erase(key) does not say which element is erased
int FN_map_erase_key_reference_bad(std::map<int, int>& map) {
  const int& value = map.at(1);
  map.erase(1);
  return value;
}

// the presence of individual keys is not tracked
int FN_map_find_absent_key_bad() {
  std::map<int, int> map;
  map[2] = 3;
  return map.find(1)->second;
}

void clear_map(std::map<int, int>& map) { map.clear(); }

// clear() in a callee does not know the elements that the caller has seen, so
// it does not invalidate references to them
int FN_call_clear_after_at_reference_bad(std::map<int, int>& map) {
  const int& value = map.at(1);
  clear_map(map);
  return value;
}

int begin_value(std::set<int>& set) { return *set.begin(); }

// the emptiness of a container is only used when it is known in the procedure
// that calls begin()
int FN_call_begin_value_empty_set_bad() {
  std::set<int> set;
  return begin_value(set);
}

// what a closure passed by value to an unknown function captures by reference
// is not havocked, so the container is still known to be empty
int FP_set_for_each_insert_ok() {
  int a[] = {1, 2};
  std::set<int> set;
  std::for_each(a, a + 2, [&set](int x) { set.insert(x); });
  return *set.begin();
}

// the end iterator of a list or of a tree-based container stays valid after
// clear(), but clear() invalidates all the iterators in the models
int FP_list_end_after_clear_ok() {
  std::list<int> list;
  auto end = list.end();
  list.clear();
  list.push_back(1);
  --end;
  return *end;
}

int FP_map_end_after_clear_ok() {
  std::map<int, int> map;
  auto end = map.end();
  map.clear();
  map[1] = 2;
  --end;
  return end->second;
}

struct DerivedMap : std::map<int, int> {};

int derived_map_subscript_ok(DerivedMap& map) {
  map[1] = 2;
  return map.find(1)->second;
}
