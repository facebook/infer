/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <algorithm>
#include <initializer_list>
#include <unordered_map>
#include <utility>
#include <type_traits>

// Keep a simplified skeleton of F14 maps for testing.
namespace folly {
struct F14HashToken {};

namespace f14::detail {
template <typename ValuePtr>
struct ValueContainerIterator {
  using pointee = typename std::pointer_traits<ValuePtr>::element_type;
  using pointer = ValuePtr;
  using reference = pointee&;

  reference operator*() const;
  pointer operator->() const;
  ValueContainerIterator& operator++();
  friend bool operator==(ValueContainerIterator const& lhs,
                         ValueContainerIterator const& rhs);
  friend bool operator!=(ValueContainerIterator const& lhs,
                         ValueContainerIterator const& rhs);
};

template <typename ValuePtr>
struct VectorContainerIterator {
  using pointee = typename std::pointer_traits<ValuePtr>::element_type;
  using pointer = ValuePtr;
  using reference = pointee&;

  reference operator*() const;
  pointer operator->() const;
  VectorContainerIterator& operator++();
  friend bool operator==(VectorContainerIterator const& lhs,
                         VectorContainerIterator const& rhs);
  friend bool operator!=(VectorContainerIterator const& lhs,
                         VectorContainerIterator const& rhs);
};

template <typename Key, typename Mapped, typename Iter, typename ConstIter>
struct F14BasicMap {
  using key_type = Key;
  using mapped_type = Mapped;
  using value_type = std::pair<const Key, Mapped>;
  using size_type = std::size_t;
  using iterator = Iter;
  using const_iterator = ConstIter;

  F14BasicMap() noexcept;

  explicit F14BasicMap(std::size_t initialCapacity);

  template <typename InputIt>
  F14BasicMap(InputIt first, InputIt last, std::size_t initialCapacity = 0);

  F14BasicMap(F14BasicMap const& rhs);

  F14BasicMap(F14BasicMap&& rhs);

  F14BasicMap(std::initializer_list<value_type> init,
              std::size_t initialCapacity = 0);

  F14BasicMap& operator=(F14BasicMap const&);

  F14BasicMap& operator=(F14BasicMap&&);

  F14BasicMap& operator=(std::initializer_list<value_type> ilist);

  iterator begin() noexcept;
  const_iterator begin() const noexcept;
  const_iterator cbegin() const noexcept;
  iterator end() noexcept;
  const_iterator end() const noexcept;
  const_iterator cend() const noexcept;

  void clear() noexcept;

  std::pair<iterator, bool> insert(value_type const& value);
  std::pair<iterator, bool> insert(value_type&& value);
  iterator insert(const_iterator /*hint*/, value_type const& value);
  iterator insert(const_iterator /*hint*/, value_type&& value);

  template <class... Args>
  iterator emplace_hint(const_iterator /*hint*/, Args&&... args);

  template <class InputIt>
  void insert(InputIt first, InputIt last);
  void insert(std::initializer_list<value_type> ilist);

  template <typename M>
  std::pair<iterator, bool> insert_or_assign(key_type const& key, M&& obj);

  template <typename M>
  std::pair<iterator, bool> insert_or_assign(key_type&& key, M&& obj);

  template <typename M>
  std::pair<iterator, bool> insert_or_assign(F14HashToken const& token,
                                             key_type const& key,
                                             M&& obj);

  template <typename M>
  std::pair<iterator, bool> insert_or_assign(F14HashToken const& token,
                                             key_type&& key,
                                             M&& obj);

  template <typename M>
  iterator insert_or_assign(const_iterator /*hint*/,
                            key_type const& key,
                            M&& obj);

  template <typename M>
  iterator insert_or_assign(const_iterator /*hint*/, key_type&& key, M&& obj);

  template <typename... Args>
  std::pair<iterator, bool> emplace(Args&&... args);

  template <typename... Args>
  std::pair<iterator, bool> try_emplace(key_type const& key, Args&&... args);

  template <typename... Args>
  std::pair<iterator, bool> try_emplace(key_type&& key, Args&&... args);

  template <typename... Args>
  std::pair<iterator, bool> try_emplace_token(F14HashToken const& token,
                                              key_type const& key,
                                              Args&&... args);

  template <typename... Args>
  std::pair<iterator, bool> try_emplace_token(F14HashToken const& token,
                                              key_type&& key,
                                              Args&&... args);

  template <typename... Args>
  iterator try_emplace(const_iterator /*hint*/,
                       key_type const& key,
                       Args&&... args);

  template <typename... Args>
  iterator try_emplace(const_iterator /*hint*/, key_type&& key, Args&&... args);

  iterator erase(const_iterator pos);

  iterator erase(iterator pos);

  iterator erase(const_iterator first, const_iterator last);

  size_type erase(key_type const& key);

  template <typename BeforeDestroy>
  iterator eraseInto(const_iterator pos, BeforeDestroy&& beforeDestroy);

  template <typename BeforeDestroy>
  iterator eraseInto(iterator pos, BeforeDestroy&& beforeDestroy);

  template <typename BeforeDestroy>
  iterator eraseInto(const_iterator first,
                     const_iterator last,
                     BeforeDestroy&& beforeDestroy);

  template <typename BeforeDestroy>
  size_type eraseInto(key_type const& key, BeforeDestroy&& beforeDestroy);

  mapped_type& at(key_type const& key);

  mapped_type const& at(key_type const& key) const;

  mapped_type& operator[](key_type const& key);

  mapped_type& operator[](key_type&& key);

  size_type count(key_type const& key) const;

  F14HashToken prehash(key_type const& key) const;

  iterator find(key_type const& key);
  const_iterator find(key_type const& key) const;
  iterator find(F14HashToken const& token, key_type const& key);
  const_iterator find(F14HashToken const& token, key_type const& key) const;

  bool contains(key_type const& key) const;
  bool contains(F14HashToken const& token, key_type const& key) const;

  std::pair<iterator, iterator> equal_range(key_type const& key);

  std::pair<const_iterator, const_iterator> equal_range(
      key_type const& key) const;

  void rehash(std::size_t bucketCapacity);

  void reserve(std::size_t capacity);

  std::size_t size() const;
};

template <typename Key, typename Mapped>
class F14VectorMapImpl
    : public F14BasicMap<
          Key,
          Mapped,
          VectorContainerIterator<std::pair<Key const, Mapped>*>,
          VectorContainerIterator<std::pair<Key const, Mapped> const*>> {
  using F14BasicMap<Key,
                    Mapped,
                    VectorContainerIterator<std::pair<Key const, Mapped>*>,
                    VectorContainerIterator<
                        std::pair<Key const, Mapped> const*>>::F14BasicMap;
};
} // namespace f14::detail

template <typename Key, typename Mapped>
struct F14ValueMap
    : public f14::detail::F14BasicMap<
          Key,
          Mapped,
          f14::detail::ValueContainerIterator<std::pair<Key const, Mapped>*>,
          f14::detail::ValueContainerIterator<
              std::pair<Key const, Mapped> const*>> {
  using f14::detail::F14BasicMap<
      Key,
      Mapped,
      f14::detail::ValueContainerIterator<std::pair<Key const, Mapped>*>,
      f14::detail::ValueContainerIterator<
          std::pair<Key const, Mapped> const*>>::F14BasicMap;

  void swap(F14ValueMap& rhs);
};

template <typename Key, typename Mapped>
struct F14VectorMap : public f14::detail::F14VectorMapImpl<Key, Mapped> {
  using f14::detail::F14VectorMapImpl<Key, Mapped>::F14VectorMapImpl;

  void swap(F14VectorMap& rhs);
};

template <typename Key, typename Mapped>
struct F14FastMap : public std::conditional<
                        sizeof(std::pair<Key const, Mapped>) < 24,
                        F14ValueMap<Key, Mapped>,
                        f14::detail::F14VectorMapImpl<Key, Mapped>>::type {
  using Super = typename std::conditional<
      sizeof(std::pair<Key const, Mapped>) < 24,
      F14ValueMap<Key, Mapped>,
      f14::detail::F14VectorMapImpl<Key, Mapped>>::type;
  using Super::Super;

  void swap(F14FastMap& rhs);
};
} // namespace folly

// sizeof(std::pair<const BigPoint, T>) >= 24.
struct BigPoint {
  std::uint64_t x, y, z;
  friend BigPoint operator+(const BigPoint& a, const BigPoint& b) {
    return {a.x + b.x, a.y + b.y, a.z + b.z};
  }
  friend bool operator==(const BigPoint& a, const BigPoint& b);
};

template <>
struct std::hash<BigPoint> {
  std::size_t operator()(const BigPoint& point) const;
};

void unordered_map_ok() {
  std::unordered_map<int, int> map = {{1, 1}, {2, 4}, {3, 9}};

  // Obtain long-lived references to keys and values.
  const auto& keyRef = map.begin()->first;
  const auto& valueRef = map.begin()->second;

  // Possible rehash, but std::unordered_map guarantees reference stability.
  map[4] = 16;
  const auto keyCopy = keyRef;
}

void folly_fastmap_bad() {
  folly::F14FastMap<int, int> map = {{1, 1}, {2, 4}, {3, 9}};

  // Obtain long-lived references to keys and values.
  const auto& keyRef = map.begin()->first;
  const auto& valueRef = map.begin()->second;

  // Possible rehash, references invalidated.
  map.emplace(4, 16);
  const auto keyCopy = keyRef;
}

void folly_fastmap_short_lived_ok() {
  folly::F14FastMap<int, int> map = {{1, 1}, {2, 4}, {3, 9}};

  {
    // Use short-lived references in a limited scope.
    const auto& keyRef = map.begin()->first;
    const auto& valueRef = map.begin()->second;

    const auto keyCopy = keyRef;
    const auto valueCopy = valueRef;

    // No need to access keyRef and valueRef outside this block.
    // Short-lived references are still valid here.
  }

  // Modify the map (no potential issues with short-lived references).
  map[4] = 16;

  // Access elements using iterators (no long-lived references).
  for (const auto& pair : map) {
    const auto keyCopy = pair.first;
    const auto valueCopy = pair.second;
  }
}

void long_lived_but_unused_ref_ok() {
  folly::F14FastMap<int, int> map = {{1, 1}, {2, 4}, {3, 9}};

  const auto& value = map.at(1);

  map.reserve(100);

  // value should be marked as invalidated here, but no error reported.
}

// We know there is no growth, so iterators/references wouldn't be invalidated.
void no_growth_ok_FP() {
  folly::F14FastMap<int, int> map = {{1, 1}, {2, 4}, {3, 9}};

  const auto& keyRef = map.at(1);

  // We know that 1 is already a key in the map, so there won't be a rehash.
  map[1] = 1;

  const auto valueCopy = keyRef;
}

void folly_valuemap_bad() {
  folly::F14ValueMap<int, int> map = {{1, 1}, {2, 4}, {3, 9}};
  const auto& valueRef = map.at(1);
  map.clear();
  const auto valueCopy = valueRef;
}

void folly_vectormap_bad() {
  folly::F14VectorMap<int, int> map = {{1, 1}, {2, 4}, {3, 9}};
  const auto& valueRef = map.at(1);
  map.clear();
  const auto valueCopy = valueRef;
}

void folly_fastmap_clear_bad() {
  folly::F14FastMap<int, int> map = {{1, 1}, {2, 4}, {3, 9}};
  const auto& valueRef = map.at(1);
  map.clear();
  const auto valueCopy = valueRef;
}

void folly_fastmap_rehash_bad() {
  folly::F14FastMap<int, int> map = {{1, 1}, {2, 4}, {3, 9}};
  const auto& valueRef = map.at(1);
  map.rehash(13);
  const auto valueCopy = valueRef;
}

void folly_fastmap_reserve_bad() {
  folly::F14FastMap<int, int> map = {{1, 1}, {2, 4}, {3, 9}};
  const auto& valueRef = map.at(1);
  map.reserve(13);
  const auto valueCopy = valueRef;
}

void folly_fastmap_operator_equal_bad() {
  folly::F14FastMap<int, int> map = {{1, 1}, {2, 4}, {3, 9}};
  const auto& valueRef = map.at(1);
  map = folly::F14FastMap<int, int>();
  const auto valueCopy = valueRef;
}

void folly_fastmap_insert_bad() {
  folly::F14FastMap<int, int> map = {{1, 1}, {2, 4}, {3, 9}};
  const auto& valueRef = map.at(1);
  map.insert({4, 16});
  const auto valueCopy = valueRef;
}

void folly_fastmap_hinted_insert_bad() {
  folly::F14FastMap<int, int> map = {{1, 1}, {2, 4}, {3, 9}};
  const auto& valueRef = map.at(1);
  map.insert(map.cbegin(), {4, 16});
  const auto valueCopy = valueRef;
}

void use_return_insert_bad() {
  folly::F14FastMap<int, int> map = {{1, 1}, {2, 4}, {3, 9}};
  const auto [it, inserted] = map.insert({4, 16});
  map.clear();
  const auto valueCopy = it->second;
}

void use_iterator_hinted_insert_bad() {
  folly::F14FastMap<int, int> map = {{1, 1}, {2, 4}, {3, 9}};
  const auto it = map.insert(map.cbegin(), {4, 16});
  map.clear();
  const auto valueCopy = it->second;
}

void insert_initializer_list_bad() {
  folly::F14FastMap<int, int> map = {{1, 1}, {2, 4}, {3, 9}};
  const auto& valueRef = map.at(1);
  map.insert({{4, 16}, {5, 25}});
  const auto valueCopy = valueRef;
}

void insert_range_bad(const std::initializer_list<std::pair<int, int>>& list) {
  folly::F14FastMap<int, int> map = {{1, 1}, {2, 4}, {3, 9}};
  const auto& valueRef = map.at(1);
  map.insert(list.begin(), list.end());
  const auto valueCopy = valueRef;
}

void insert_range_from_map_bad(const folly::F14FastMap<int, int>& other) {
  folly::F14FastMap<int, int> map = {{1, 1}, {2, 4}, {3, 9}};
  const auto& valueRef = map.at(1);
  map.insert(other.begin(), other.end());
  const auto valueCopy = valueRef;
}

void folly_fastmap_insert_or_assign_bad() {
  folly::F14FastMap<int, int> map = {{1, 1}, {2, 4}, {3, 9}};
  const auto& valueRef = map.at(1);
  map.insert_or_assign(4, 16);
  const auto valueCopy = valueRef;
}

void folly_fastmap_token_insert_or_assign_bad() {
  folly::F14FastMap<int, int> map = {{1, 1}, {2, 4}, {3, 9}};
  const auto& valueRef = map.at(1);
  map.insert_or_assign(map.prehash(4), 4, 16);
  const auto valueCopy = valueRef;
}

void folly_fastmap_hinted_insert_or_assign_bad() {
  folly::F14FastMap<int, int> map = {{1, 1}, {2, 4}, {3, 9}};
  const auto& valueRef = map.at(1);
  map.insert_or_assign(map.cbegin(), 4, 16);
  const auto valueCopy = valueRef;
}

void use_return_insert_or_assign_bad() {
  folly::F14FastMap<int, int> map = {{1, 1}, {2, 4}, {3, 9}};
  const auto [it, inserted] = map.insert_or_assign(4, 16);
  map.clear();
  const auto valueCopy = it->second;
}

void use_return_token_insert_or_assign_bad() {
  folly::F14FastMap<int, int> map = {{1, 1}, {2, 4}, {3, 9}};
  const auto [it, inserted] = map.insert_or_assign(map.prehash(4), 4, 16);
  map.clear();
  const auto valueCopy = it->second;
}

void use_iterator_hinted_insert_or_assign_bad() {
  folly::F14FastMap<int, int> map = {{1, 1}, {2, 4}, {3, 9}};
  const auto it = map.insert_or_assign(map.cbegin(), 4, 16);
  map.clear();
  const auto valueCopy = it->second;
}

void folly_fastmap_emplace_bad() {
  folly::F14FastMap<int, int> map = {{1, 1}, {2, 4}, {3, 9}};
  const auto& valueRef = map.at(1);
  map.emplace(4, 16);
  const auto valueCopy = valueRef;
}

void folly_fastmap_try_emplace_bad() {
  folly::F14FastMap<int, int> map = {{1, 1}, {2, 4}, {3, 9}};
  const auto& valueRef = map.at(1);
  map.try_emplace(4, 16);
  const auto valueCopy = valueRef;
}

void folly_fastmap_hinted_try_emplace_bad() {
  folly::F14FastMap<int, int> map = {{1, 1}, {2, 4}, {3, 9}};
  const auto& valueRef = map.at(1);
  map.try_emplace(map.cbegin(), 4, 16);
  const auto valueCopy = valueRef;
}

void use_try_emplace_return_bad() {
  folly::F14FastMap<int, int> map = {{1, 1}, {2, 4}, {3, 9}};
  const auto [it, inserted] = map.try_emplace(4, 16);
  map.clear();
  const auto valueCopy = it->second;
}

void use_hinted_try_emplace_iterator_bad() {
  folly::F14FastMap<int, int> map = {{1, 1}, {2, 4}, {3, 9}};
  const auto it = map.try_emplace(map.cbegin(), 4, 16);
  map.clear();
  const auto valueCopy = it->second;
}

void folly_fastmap_try_emplace_token_bad() {
  folly::F14FastMap<int, int> map = {{1, 1}, {2, 4}, {3, 9}};
  const auto& valueRef = map.at(1);
  map.try_emplace_token(map.prehash(4), 4, 16);
  const auto valueCopy = valueRef;
}

void folly_fastmap_emplace_hint_bad() {
  folly::F14FastMap<int, int> map = {{1, 1}, {2, 4}, {3, 9}};
  const auto& valueRef = map.at(1);
  map.emplace_hint(map.cbegin(), 4, 16);
  const auto valueCopy = valueRef;
}

void use_iterator_emplace_hint_bad() {
  folly::F14FastMap<int, int> map = {{1, 1}, {2, 4}, {3, 9}};
  const auto it = map.emplace_hint(map.cbegin(), 4, 16);
  map.clear();
  const auto valueCopy = it->second;
}

void folly_fastmap_operator_bracket_bad() {
  folly::F14FastMap<int, int> map = {{1, 1}, {2, 4}, {3, 9}};
  const auto& valueRef = map.at(1);
  map[4] = 16;
  const auto valueCopy = valueRef;
}

void use_reference_operator_bracket_bad() {
  folly::F14FastMap<int, int> map = {{1, 1}, {2, 4}, {3, 9}};
  const auto& valueRef = map[1];
  map[4] = 16;
  const auto valueCopy = valueRef;
}

void folly_fastmap_erase_bad_FN() {
  folly::F14FastMap<int, int> map = {{1, 1}, {2, 4}, {3, 9}};
  const auto& valueRef = map.at(1);
  map.erase(1);
  const auto valueCopy = valueRef;
}

void folly_fastmap_swap_ok() {
  folly::F14FastMap<int, int> map = {{1, 1}, {2, 4}, {3, 9}};
  folly::F14FastMap<int, int> other;
  const auto& valueRef = map.at(1);
  map.swap(other); // valueRef now valid in other.
  const auto valueCopy = valueRef;
}

void folly_fastmap_swap_bad() {
  folly::F14FastMap<int, int> map = {{1, 1}, {2, 4}, {3, 9}};
  folly::F14FastMap<int, int> other;
  const auto& valueRef = map.at(1);
  map.swap(other); // valueRef now valid in other.
  other.clear(); // valueRef now invalid.
  const auto valueCopy = valueRef;
}

void folly_valuemap_find_arrow_bad() {
  folly::F14ValueMap<int, int> map = {{1, 1}, {2, 4}, {3, 9}};
  const auto it = map.find(1);
  const auto& valueRef = it->second;
  map.clear();
  const auto valueCopy = valueRef;
}

void folly_valuemap_find_star_bad() {
  folly::F14ValueMap<int, int> map = {{1, 1}, {2, 4}, {3, 9}};
  const auto it = map.find(1);
  const auto& keyRef = (*it).first;
  map.clear();
  const auto keyCopy = keyRef;
}

void folly_vectormap_find_arrow_bad() {
  folly::F14VectorMap<int, int> map = {{1, 1}, {2, 4}, {3, 9}};
  const auto it = map.find(1);
  const auto& valueRef = it->second;
  map.clear();
  const auto valueCopy = valueRef;
}

void folly_vectormap_find_star_bad() {
  folly::F14VectorMap<int, int> map = {{1, 1}, {2, 4}, {3, 9}};
  const auto it = map.find(1);
  const auto& keyRef = (*it).first;
  map.clear();
  const auto keyCopy = keyRef;
}

void folly_fastmap_find_arrow_bad() {
  // The underlying map is an F14ValueMap.
  folly::F14FastMap<int, int> map = {{1, 1}, {2, 4}, {3, 9}};
  const auto it = map.find(1);
  const auto& valueRef = it->second;
  map.clear();
  const auto valueCopy = valueRef;
}

void folly_fastmap_find_star_bad() {
  // The underlying map is an F14ValueMap.
  folly::F14FastMap<int, int> map = {{1, 1}, {2, 4}, {3, 9}};
  const auto it = map.find(1);
  const auto& keyRef = (*it).first;
  map.clear();
  const auto keyCopy = keyRef;
}

void folly_fastmap_big_find_arrow_bad(folly::F14FastMap<BigPoint, int>& map) {
  // The underlying map is an F14VectorMap.
  const auto it = map.find({0, 0, 0});
  const auto& valueRef = it->second;
  map.clear();
  const auto valueCopy = valueRef;
}

void folly_fastmap_big_find_star_bad(folly::F14FastMap<BigPoint, int>& map) {
  // The underlying map is an F14VectorMap.
  const auto it = map.find({0, 0, 0});
  const auto& keyRef = (*it).first;
  map.clear();
  const auto keyCopy = keyRef;
}

void folly_fastmap_begin_bad(folly::F14FastMap<int, int>& map) {
  auto it = map.begin();
  map.clear();
  ++it->second;
}

void folly_fastmap_cbegin_bad(folly::F14FastMap<int, int>& map) {
  const auto it = map.cbegin();
  map.clear();
  const auto keyCopy = it->first;
}

void folly_fastmap_iterator_increment_bad_FN(folly::F14FastMap<int, int>& map) {
  auto it = map.begin();
  ++it;
  const auto& valueRef = it->second;
  map.clear();
  const auto valueCopy = valueRef;
}

void iterator_copy_constructor_bad(folly::F14FastMap<int, int>& map) {
  const auto it = map.begin();
  const auto it2 = it;
  map.clear();
  const auto keyCopy = it2->first;
}

void iterator_copy_operator_equal_bad(folly::F14FastMap<int, int>& map) {
  const auto it = map.begin();
  folly::F14FastMap<int, int>::iterator it2;
  it2 = it;
  map.clear();
  const auto keyCopy = it2->first;
}

void weird_operator_bracket_bad_FN(folly::F14FastMap<int, int>& map) {
  // This is not valid, as the map may resize for the insert call prior to
  // accessing map[71] and constructing the pair.
  map.emplace(13, map[71]);
}

// https://github.com/facebook/folly/blob/1cf9ac0/folly/container/F14.md?plain=1#L293-L296
void reserve_operator_bracket_ok_FP(folly::F14FastMap<int, int>& map) {
  map.reserve(map.size() + 2);
  const auto& r1 = map[13];
  const auto& r2 = map[71];
  const auto r1Copy = r1;
}

void use_emplace_iterator_bad(folly::F14FastMap<int, int>& map) {
  const auto [it, inserted] = map.emplace(13, 71);
  map.clear();
  // Copy here is fine, iterator is invalid but we are not accessing it.
  const auto copy = it;
  const auto value = copy->second;
}

void multiple_lookups_existing_key_ok(folly::F14FastMap<int, int>& map) {
  const auto& valueRef = map.at(71);
  const auto key = 13;
  if (map.contains(key)) {
    map[key] = 17;
    map[key] = 31;
    map[key] = 71;
  }
  const auto valueCopy = valueRef;
}

void known_existing_map_key_count_ok(folly::F14FastMap<int, int>& map) {
  const auto& valueRef = map.at(71);
  const auto key = 13;
  if (map.count(key) != 0) {
    map[key] = std::max(map[key], 17);
  }
  const auto valueCopy = valueRef;
}

void known_existing_map_key_contains_ok(folly::F14FastMap<int, int>& map) {
  const auto& valueRef = map.at(71);
  const auto key = 13;
  if (map.contains(key)) {
    map[key] = std::max(map[key], 17);
  }
  const auto valueCopy = valueRef;
}

void known_existing_map_key_find_ok(folly::F14FastMap<int, int>& map) {
  const auto& valueRef = map.at(71);
  const auto key = 13;
  if (map.find(key) != map.end()) {
    map[key] = std::max(map[key], 17);
  }
  const auto valueCopy = valueRef;
}

void double_lookup_not_found_bad_FN(folly::F14FastMap<int, int>& map) {
  const auto& valueRef = map.at(71);
  const auto key = 13;
  if (!map.contains(key)) {
    map[key] = 17;
  }
  const auto valueCopy = valueRef;
}

void known_existing_map_key_literal_ok_FP(folly::F14FastMap<int, int>& map) {
  const auto& valueRef = map.at(71);
  if (map.contains(13)) {
    map[13] = std::max(map[13], 17);
  }
  const auto valueCopy = valueRef;
}

void delete_in_loop_ok(folly::F14FastMap<int, int*>& map) {
  for (auto& it : map) {
    delete it.second;
  }
}

void right_sequenced_before_left_ok(folly::F14FastMap<int, int>& map) {
  // > The assignment operator (=) and the compound assignment operators all
  // > group right-to-left. The right operand is sequenced before the left
  // > operand.
  // -- https://wg21.link/N4950, section 7.6.19 [expr.ass].
  map[13] = map[71] / 31;
}

void right_before_left_compound_ok(folly::F14FastMap<int, int>& map) {
  map[13] += map[71] / 31;
}

// This only seems to be a false negative for integral types.
void unsafe_assign_bad_FN(folly::F14FastMap<int, int>& map) {
  // The LHS evaluation can invalidate the RHS reference.
  map[13] = map[71];
}

void unsafe_operation_bad_FN(folly::F14FastMap<int, int>& map) {
  const auto result = map[13] * map[71];
}

void unsafe_assign_struct_bad(folly::F14FastMap<int, BigPoint>& map) {
  map[13] = map[71];
}

void unsafe_operation_struct_bad(folly::F14FastMap<int, BigPoint>& map) {
  const auto result = map[13] + map[71];
}

int mul(const int& a, const int& b) { return a * b; }

void unsafe_function_call_bad(folly::F14FastMap<int, int>& map) {
  const auto result = mul(map[13], map[71]);
}

void unsafe_function_call_lambda_bad(folly::F14FastMap<int, int>&& map) {
  [&]() { const auto result = mul(map[13], map[71]); };
}
