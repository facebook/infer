/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <cstdint>
#include <memory>
#include <stdexcept>

extern int __infer_taint_source();

namespace std {
template <class T>
unique_ptr<T> make_unique(size_t n) {
  typedef typename remove_extent<T>::type U;
  return unique_ptr<T>(new U[n]());
}
} // namespace std

namespace Codec_Bad {
uint32_t getP_Bad(uint32_t w) {
  auto w4 = w * 4; // BUG: can overflow
  auto w4m1 = w4 - 1; // BUG: can underflow (if w = 0)
  auto w4m1o15 = w4m1 | 15; // ALWAYS OK
  auto w4m1o15p1 = w4m1o15 + 1; // BUG: can overflow
  return w4m1o15p1;
}
void foo_Bad_FN() {
  int w = __infer_taint_source();
  int h = __infer_taint_source();
  auto p =
      getP_Bad(w); // MISSED BUG: downcasting signed int64 -> unsigned int32
  auto s = h * p; // BUG: multiplication can overflow
  auto d = std::make_unique<uint8_t[]>(s); // MISSED BUG: casting signed int64
                                           // -> unsigned int64,
}
} // namespace Codec_Bad

namespace Codec_Bad2 {
uint64_t getP_Bad(uint64_t w) {
  auto w4 = w * 4; // BUG: can overflow
  auto w4m1 = w4 - 1; // BUG: can underflow (if w = 0)
  auto w4m1o15 = w4m1 | 15; // ALWAYS OK
  auto w4m1o15p1 = w4m1o15 + 1; // BUG: can overflow
  return w4m1o15p1;
}
uint64_t checkedMultiply_Good_FP(uint64_t a, uint64_t b) {
  __uint128_t mul = ((__uint128_t)a) * b; // OK: no overflow
  if ((mul >> 64) != 0) {
    throw std::runtime_error("Detected overflow in checked multiplcation");
  }
  auto result = (uint64_t)mul; // OK: within the bounds
  return result;
}
void foo_Bad_FN() {
  int w = __infer_taint_source();
  int h = __infer_taint_source();
  auto p = getP_Bad(w); // MISSED BUG: casting signed int64 -> unsigned int64
  auto s = checkedMultiply_Good_FP(h, p); // OK
  auto d = std::make_unique<uint8_t[]>(s); // OK
}
} // namespace Codec_Bad2
