/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
namespace Foo {

template <typename T, bool>
struct is_negative_impl {
  constexpr static bool check(T x) { return x < 0; }
};

template <typename T>
struct is_negative_impl<T, false> {
  constexpr static bool check(T) { return false; }
};

} // namespace Foo
