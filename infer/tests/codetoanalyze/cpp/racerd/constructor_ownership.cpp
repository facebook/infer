/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <mutex>
#include <string>
#include <unordered_map>

namespace constructors {

struct dynamic {
  enum T {
    NULLT,
  };

 private:
  struct ObjectMaker;

 public:
  template <class T>
  dynamic(T t);
  static ObjectMaker object();
  dynamic(dynamic&&) noexcept;
  dynamic& operator=(dynamic&&) noexcept;
  dynamic& operator[](dynamic const&) &;
  dynamic&& operator[](dynamic const&) &&;
  template <class K, class V>
  void insert(K&&, V&& val);

 private:
  T type_;
};

dynamic& dynamic::operator=(dynamic&& o) noexcept {
  if (&o != this) {
    if (type_ == o.type_) {
    } else {
      type_ = o.type_;
    }
  }
  return *this;
}

struct dynamic::ObjectMaker {
  friend struct dynamic;

  explicit ObjectMaker() {}

  ObjectMaker(ObjectMaker&&) = default;
};

inline dynamic::ObjectMaker dynamic::object() { return ObjectMaker(); }

struct BSS {
  dynamic toJson_ok() const noexcept {
    dynamic ret = dynamic::object();
    ret["key"] = dynamic::object();
    return ret;
  }

  dynamic& toJson_race(dynamic& ret) const noexcept {
    ret["key"] = dynamic::object();
    return ret;
  }
};

struct TSL {
  std::mutex mutex_;

  void not_locked_ok(dynamic& ret) { BSS().toJson_ok(); }

  void locked_ok(dynamic& ret) {
    std::lock_guard<std::mutex> lock(mutex_);
    BSS().toJson_ok();
  }

  void FN_not_locked_race(dynamic& ret) { BSS().toJson_race(ret); }

  void locked_race(dynamic& ret) {
    std::lock_guard<std::mutex> lock(mutex_);
    BSS().toJson_race(ret);
  }
};

} // namespace constructors
