/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <initializer_list>
#include <unordered_map>
#include <utility>
#include <type_traits>

// Keep a simplified skeleton of F14 maps for testing.
namespace folly {
struct F14HashToken;

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

  F14HashToken prehash(key_type const& key) const;

  iterator find(key_type const& key);

  const_iterator find(key_type const& key) const;

  iterator find(F14HashToken const& token, key_type const& key);

  const_iterator find(F14HashToken const& token, key_type const& key) const;

  std::pair<iterator, iterator> equal_range(key_type const& key);

  std::pair<const_iterator, const_iterator> equal_range(
      key_type const& key) const;

  void rehash(std::size_t bucketCapacity);

  void reserve(std::size_t capacity);
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

void unordered_map_ok() {
  std::unordered_map<int, int> map = {{1, 1}, {2, 4}, {3, 9}};

  // Obtain long-lived references to keys and values.
  const auto& keyRef = map.begin()->first;
  const auto& valueRef = map.begin()->second;

  // Possible rehash, but std::unordered_map guarantees reference stability.
  map[4] = 16;
  const auto keyCopy = keyRef;
}

void folly_fastmap_bad_FN() {
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
void no_growth_ok() {
  folly::F14FastMap<int, int> map = {{1, 1}, {2, 4}, {3, 9}};

  const auto& keyRef = map.at(1);

  // We know that 1 is already a key in the map, so there won't be a rehash.
  map[1] = 1;

  const auto valueCopy = keyRef;
}

void folly_valuemap_bad_FN() {
  folly::F14ValueMap<int, int> map = {{1, 1}, {2, 4}, {3, 9}};
  const auto& valueRef = map.at(1);
  map.clear();
  const auto valueCopy = valueRef;
}

void folly_vectormap_bad_FN() {
  folly::F14VectorMap<int, int> map = {{1, 1}, {2, 4}, {3, 9}};
  const auto& valueRef = map.at(1);
  map.clear();
  const auto valueCopy = valueRef;
}

void folly_fastmap_clear_bad_FN() {
  folly::F14FastMap<int, int> map = {{1, 1}, {2, 4}, {3, 9}};
  const auto& valueRef = map.at(1);
  map.clear();
  const auto valueCopy = valueRef;
}

void folly_fastmap_rehash_bad_FN() {
  folly::F14FastMap<int, int> map = {{1, 1}, {2, 4}, {3, 9}};
  const auto& valueRef = map.at(1);
  map.rehash(13);
  const auto valueCopy = valueRef;
}

void folly_fastmap_reserve_bad_FN() {
  folly::F14FastMap<int, int> map = {{1, 1}, {2, 4}, {3, 9}};
  const auto& valueRef = map.at(1);
  map.reserve(13);
  const auto valueCopy = valueRef;
}

void folly_fastmap_operator_equal_bad_FN() {
  folly::F14FastMap<int, int> map = {{1, 1}, {2, 4}, {3, 9}};
  const auto& valueRef = map.at(1);
  map = folly::F14FastMap<int, int>();
  const auto valueCopy = valueRef;
}

void folly_fastmap_insert_bad_FN() {
  folly::F14FastMap<int, int> map = {{1, 1}, {2, 4}, {3, 9}};
  const auto& valueRef = map.at(1);
  map.insert({4, 16});
  const auto valueCopy = valueRef;
}

void folly_fastmap_insert_or_assign_bad_FN() {
  folly::F14FastMap<int, int> map = {{1, 1}, {2, 4}, {3, 9}};
  const auto& valueRef = map.at(1);
  map.insert_or_assign(4, 16);
  const auto valueCopy = valueRef;
}

void folly_fastmap_emplace_bad_FN() {
  folly::F14FastMap<int, int> map = {{1, 1}, {2, 4}, {3, 9}};
  const auto& valueRef = map.at(1);
  map.emplace(4, 16);
  const auto valueCopy = valueRef;
}

void folly_fastmap_try_emplace_bad_FN() {
  folly::F14FastMap<int, int> map = {{1, 1}, {2, 4}, {3, 9}};
  const auto& valueRef = map.at(1);
  map.try_emplace(4, 16);
  const auto valueCopy = valueRef;
}

void folly_fastmap_emplace_hint_bad_FN() {
  folly::F14FastMap<int, int> map = {{1, 1}, {2, 4}, {3, 9}};
  const auto& valueRef = map.at(1);
  map.emplace_hint(map.cbegin(), 4, 16);
  const auto valueCopy = valueRef;
}

void folly_fastmap_operator_bracket_bad_FN() {
  folly::F14FastMap<int, int> map = {{1, 1}, {2, 4}, {3, 9}};
  const auto& valueRef = map.at(1);
  map[4] = 16;
  const auto valueCopy = valueRef;
}

void folly_fastmap_erase_bad_FN() {
  folly::F14FastMap<int, int> map = {{1, 1}, {2, 4}, {3, 9}};
  const auto& valueRef = map.at(1);
  map.erase(1);
  const auto valueCopy = valueRef;
}

void folly_fastmap_swap_bad_FN() {
  folly::F14FastMap<int, int> map = {{1, 1}, {2, 4}, {3, 9}};
  folly::F14FastMap<int, int> other;
  const auto& valueRef = map.at(1);
  map.swap(other);
  const auto valueCopy = valueRef;
}
