/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <new>
#include <utility>

// Minimal folly::Expected caricature for testing Pulse models.
// The real folly::Expected uses a discriminated union with a `which_` tag.
// Infer loses track of `which_` initialization through macro expansion
// (FOLLY_LIKELY/FOLLY_UNLIKELY) and reports FP PULSE_UNINITIALIZED_VALUE
// when hasValue/hasError/value/operator* read the tag.
namespace folly {

namespace expected_detail {

enum class Which : unsigned char { eEmpty, eValue, eError };

template <class Value, class Error>
union ExpectedUnion {
  Value value_;
  Error error_;
  char ch_;

  ExpectedUnion() : ch_{} {}

  template <class... Vs>
  explicit ExpectedUnion(Vs&&... vs) : value_(static_cast<Vs&&>(vs)...) {}
};

template <class Value, class Error>
struct ExpectedStorage {
  ExpectedUnion<Value, Error> union_;
  Which which_{Which::eEmpty};

  ExpectedStorage() = default;

  template <class... Vs>
  explicit ExpectedStorage(Vs&&... vs)
      : union_(static_cast<Vs&&>(vs)...), which_(Which::eValue) {}
};

} // namespace expected_detail

template <class Value, class Error>
class Expected {
 public:
  Expected() = default;

  template <class V>
  Expected(V&& val)
      : storage_(static_cast<V&&>(val)) {}

  constexpr bool hasValue() const noexcept {
    return storage_.which_ == expected_detail::Which::eValue;
  }

  constexpr bool hasError() const noexcept {
    return storage_.which_ == expected_detail::Which::eError;
  }

  explicit constexpr operator bool() const noexcept { return hasValue(); }

  Value& value() & { return storage_.union_.value_; }
  const Value& value() const& { return storage_.union_.value_; }

  Value& operator*() & { return value(); }

 private:
  expected_detail::ExpectedStorage<Value, Error> storage_;
};

} // namespace folly

enum class EncodingType { Raw, Compressed, Encrypted };

struct ValidationError {
  int code;
};

folly::Expected<EncodingType, ValidationError> validateEncoding(int x);

// Test: hasValue/hasError on a returned Expected should not report uninit.
void folly_expected_has_value_ok() {
  auto result = validateEncoding(1);
  if (result.hasValue()) {
    EncodingType e = result.value();
  }
}

void folly_expected_has_error_ok() {
  auto result = validateEncoding(2);
  if (result.hasError()) {
    // handle error
  }
}

void folly_expected_operator_bool_ok() {
  auto result = validateEncoding(3);
  if (result) {
    EncodingType e = *result;
  }
}

void folly_expected_value_ok() {
  auto result = validateEncoding(4);
  if (result.hasValue()) {
    EncodingType e = result.value();
  }
}
