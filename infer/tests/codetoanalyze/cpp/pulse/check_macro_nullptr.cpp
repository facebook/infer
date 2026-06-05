/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// Repro for T274312781: COMPARED_TO_NULL_AND_DEREFERENCED false positive after
// an android-base `CHECK(ptr != nullptr)`. `CHECK` aborts when the condition is
// false, so the pointer is provably non-null afterwards, yet Pulse keeps the
// null branch alive into the dereference and reports.
//
// This mirrors `system/libbase/include/android-base/logging.h` as Pulse sees it:
//
//   #define CHECK(x) \
//     LIKELY((x)) || \
//         ::android::base::LogMessage(__FILE__, __LINE__, FATAL, ...).stream() \
//             << "Check failed: " #x << " "
//
// On the failing branch a FATAL `LogMessage` temporary is constructed; its
// destructor aborts -- but only for FATAL severity and only through a
// function-pointer `Aborter()(msg)` indirection in a separate TU, a call Pulse
// cannot follow (modelled here by the declared-but-undefined `invoke_aborter`).
// So Pulse never learns the failing branch does not return.
//
// The fix models the `android::base::LogMessage` constructor: when the severity
// argument is FATAL the construction is treated as non-returning (`early_exit`),
// which prunes the failing branch before the dereference. Non-fatal severities
// (LOG(INFO) etc.) are left untouched.

namespace android {
namespace base {

enum LogSeverity { VERBOSE, DEBUG, INFO, WARNING, ERROR, FATAL_WITHOUT_ABORT, FATAL };

// Mirrors `Aborter()(msg)` in logging.cpp: the real abort is behind a function
// pointer set in another TU, so Pulse has no summary and assumes it may return.
void invoke_aborter(const char* msg);

struct LogStream {
  template <typename T>
  LogStream& operator<<(const T&) {
    return *this;
  }
  // Mirrors `std::ostream`'s contextual bool conversion, which the `||` chain in
  // the real `CHECK` relies on.
  explicit operator bool() const { return false; }
};

class LogMessage {
 public:
  LogMessage(const char* file, int line, LogSeverity severity, const char* tag,
             int error)
      : severity_(severity) {}

  // For FATAL severity this aborts, but only through the opaque `invoke_aborter`
  // indirection, so Pulse does not learn that the destructor never returns.
  ~LogMessage() {
    if (severity_ == FATAL) {
      invoke_aborter("Check failed");
    }
  }

  LogStream& stream() { return stream_; }

 private:
  LogSeverity severity_;
  LogStream stream_;
};

} // namespace base
} // namespace android

#define LIKELY(x) __builtin_expect(!!(x), 1)

#define CHECK(x)                                                          \
  LIKELY((x)) ||                                                          \
      ::android::base::LogMessage(__FILE__, __LINE__,                     \
                                 ::android::base::FATAL, "", -1)          \
          .stream()                                                       \
              << "Check failed: " #x << " "

// FALSE POSITIVE (before the LogMessage FATAL-constructor model): `CHECK` aborts
// when `p` is null, so `*p` is unreachable with a null `p`, yet Pulse reports
// COMPARED_TO_NULL_AND_DEREFERENCED here.
void check_then_deref_good_FP(int* p) {
  CHECK(p != nullptr) << "p must not be null";
  *p = 42;
}

// Control: same shape but the abort is a visible, modeled abort, so Pulse prunes
// the null branch and reports nothing. Guards against a fix that over-suppresses.
void check_visible_abort_then_deref_ok(int* p) {
  if (!(p != nullptr)) {
    __builtin_abort();
  }
  *p = 42;
}

// True positive control: a bare comparison with no abort guard must still be
// reported, so the fix must not blanket-disable the check.
void compared_then_deref_bad(int* p) {
  if (p == nullptr) {
  }
  *p = 42;
}

// Non-fatal control: constructing a non-FATAL LogMessage must NOT be treated as
// no-return, so a genuine null deref after `LOG(INFO)`-style logging is still
// found.
void log_info_then_deref_bad(int* p) {
  if (p == nullptr) {
    ::android::base::LogMessage(__FILE__, __LINE__, ::android::base::INFO, "",
                               -1)
            .stream()
        << "p is null";
  }
  *p = 42;
}
