/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <new>
#include <utility>

// Minimal folly::Try caricature for testing Pulse models.
// Mirrors the placement-new pattern in folly::detail::TryBase::emplace
// that causes Infer to lose track of field initialization.
namespace folly {

enum class Contains { NOTHING, VALUE, EXCEPTION };

namespace detail {

template <class T>
class TryBase {
 public:
  TryBase() noexcept : contains_(Contains::NOTHING) {}

  TryBase(TryBase&& t) noexcept : contains_(t.contains_) {
    if (contains_ == Contains::VALUE) {
      ::new (static_cast<void*>(&value_)) T(std::move(t.value_));
    }
  }

  template <class T2>
  explicit TryBase(T2&& v) : contains_(Contains::VALUE) {
    ::new (static_cast<void*>(&value_)) T(static_cast<T2&&>(v));
  }

  ~TryBase() {
    if (contains_ == Contains::VALUE) {
      value_.~T();
    }
  }

  void destroy() noexcept {
    if (contains_ == Contains::VALUE) {
      value_.~T();
    }
    contains_ = Contains::NOTHING;
  }

 protected:
  Contains contains_;
  union {
    T value_;
  };
};

} // namespace detail

template <class T>
class Try : public detail::TryBase<T> {
 public:
  using detail::TryBase<T>::TryBase;

  // The real folly::Try::emplace uses placement-new to construct T in-place.
  // Infer loses track of field initialization through the placement-new,
  // causing FP PULSE_UNINITIALIZED_VALUE on fields of T.
  template <typename... Args>
  T& emplace(Args&&... args) noexcept {
    this->destroy();
    ::new (static_cast<void*>(&this->value_))
        T(static_cast<Args&&>(args)...);
    this->contains_ = Contains::VALUE;
    return this->value_;
  }

  bool hasValue() const { return this->contains_ == Contains::VALUE; }

  T& value() & { return this->value_; }
};

} // namespace folly

// Mirrors manifold::MultiReadResult. Key properties:
// - Fields WITHOUT default initializers (like enums, plain ints)
// - User-provided move constructor that explicitly reads fields
// When such a struct is constructed, fields set, then moved through
// Try::emplace's placement-new, Infer loses track of the field init
// and reports the move constructor's field read as uninit.
enum class AppType { Unknown, Payment, Commerce };

struct VerificationResponse {
  AppType accountApp;
  int reasonCode;
  int status;

  VerificationResponse() = default;

  // User-provided move constructor — mirrors MultiReadResult::MultiReadResult.
  // The explicit field reads here (e.g., v.accountApp) are where Infer
  // reports the uninit value when the struct comes through placement-new.
  VerificationResponse(VerificationResponse&& v) noexcept
      : accountApp(v.accountApp),
        reasonCode(v.reasonCode),
        status(v.status) {}

  VerificationResponse& operator=(VerificationResponse&&) = default;
};

// Mirrors folly::coro::detail::TaskPromise::return_value which calls
// this->result_.emplace(std::forward<U>(value))
class TaskPromise {
 public:
  template <typename U>
  void return_value(U&& value) {
    result_.emplace(static_cast<U&&>(value));
  }

  folly::Try<VerificationResponse> result_;
};

// Reproduces the FP from payment/NmorPaymentEngine and similar:
// co_return constructs a VerificationResponse, passes it through
// TaskPromise::return_value → Try::emplace → placement-new.
// Infer loses track of field initialization through the placement-new.
void folly_try_emplace_ok() {
  VerificationResponse resp;
  resp.accountApp = AppType::Payment;
  resp.reasonCode = 0;
  resp.status = 1;
  TaskPromise promise;
  promise.return_value(std::move(resp));
  int x = promise.result_.value().reasonCode;
}

// Direct emplace with move also triggers the FP.
void folly_try_direct_emplace_ok() {
  VerificationResponse resp;
  resp.accountApp = AppType::Commerce;
  resp.reasonCode = 42;
  resp.status = 0;
  folly::Try<VerificationResponse> t;
  t.emplace(std::move(resp));
  int x = t.value().reasonCode;
}

// Constructing Try with a value directly.
void folly_try_value_constructor_ok() {
  VerificationResponse resp;
  resp.accountApp = AppType::Payment;
  resp.reasonCode = 1;
  resp.status = 2;
  folly::Try<VerificationResponse> t{std::move(resp)};
  int x = t.value().reasonCode;
}
