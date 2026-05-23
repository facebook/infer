/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// Minimal apache::thrift::field_ref caricature for testing the Pulse
// operator* and operator-> models.

namespace apache {
namespace thrift {

template <class T>
class field_ref {
 public:
  field_ref(T& value) : value_(value) {}
  T& operator*() const { return value_; }
  T* operator->() const { return &value_; }

 private:
  T& value_;
};

} // namespace thrift
} // namespace apache

struct Inner {
  int leaf;
};

void uaf_via_star_bad() {
  Inner* p = new Inner;
  auto fr = apache::thrift::field_ref<Inner>(*p);
  delete p;
  (*fr).leaf = 42;
}

void uaf_via_arrow_bad() {
  Inner* p = new Inner;
  auto fr = apache::thrift::field_ref<Inner>(*p);
  delete p;
  fr->leaf = 42;
}
