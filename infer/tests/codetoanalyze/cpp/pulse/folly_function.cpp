/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <utility>

// Minimal folly::Function caricature for testing Pulse models
namespace folly {

template <class FunctionType>
class Function;

namespace detail {
namespace function {
template <class FunctionType>
struct FunctionTraits;

template <class ReturnType, class... Args>
struct FunctionTraits<ReturnType(Args...)> {
  ReturnType operator()(Args...);
};
} // namespace function
} // namespace detail

using Exec = std::size_t (*)(int, void*, void*);

struct Data {
  void* object_;
};

template <class ReturnType, class... Args>
class Function<ReturnType(Args...)>
    : private detail::function::FunctionTraits<ReturnType(Args...)> {
 public:
  Function() noexcept : exec_(nullptr) {}

  Function(Function&& that) noexcept
      : call_(that.call_), exec_(that.exec_) {
    that.exec_ = nullptr;
  }

  template <class Fun>
  Function(Fun&& fun) : exec_(reinterpret_cast<Exec>(1)) {}

  // Mirrors real folly::Function::operator= which calls exec() helper
  Function& operator=(Function&& that) noexcept {
    destroyCurrent();
    call_ = that.call_;
    exec_ = that.exec_;
    that.exec_ = nullptr;
    return *this;
  }

  template <class Fun>
  Function& operator=(Fun&& fun) {
    destroyCurrent();
    exec_ = reinterpret_cast<Exec>(1);
    return *this;
  }

  ~Function() { destroyCurrent(); }

  explicit operator bool() const noexcept { return exec_ != nullptr; }

  using detail::function::FunctionTraits<ReturnType(Args...)>::operator();

 private:
  // Mirrors folly::Function::exec — reads exec_ to dispatch cleanup.
  // This indirection is where Infer loses track of exec_ initialization.
  void exec(int op, Data* src, Data* dst) {
    if (!exec_) {
      return;
    }
    exec_(op, src, dst);
  }

  void destroyCurrent() {
    exec(0, &data_, nullptr);
  }

  ReturnType (*call_)(Data&, Args&&...){nullptr};
  Exec exec_{nullptr};
  Data data_{};
};

} // namespace folly

enum class JobExitCode { Done, Reschedule };
using Job = folly::Function<JobExitCode()>;

struct QueueEntry {
  Job job;
  int rescheduleCount{};

  QueueEntry(Job j) : job{std::move(j)} {}
  QueueEntry(QueueEntry&&) = default;
  QueueEntry& operator=(QueueEntry&&) = default;
};

// Reproduces the FP from cachelib's ThreadPoolJobQueue: after move-constructing
// a QueueEntry, assigning Job{} to entry.job via operator= was reported as
// reading an uninitialized value (exec_ inside folly::Function).
void FP_folly_function_move_assign_ok() {
  QueueEntry entry{Job{[] { return JobExitCode::Done; }}};
  // This operator= call was previously a FP (PULSE_UNINITIALIZED_VALUE)
  // because Infer had no model for folly::Function::operator=.
  entry.job = Job{};
}

void folly_function_construct_and_call_ok() {
  folly::Function<int()> f{[] { return 42; }};
  int x = f();
}

void folly_function_move_construct_ok() {
  folly::Function<int()> f1{[] { return 42; }};
  folly::Function<int()> f2{std::move(f1)};
  int x = f2();
}

void folly_function_move_assign_call_ok() {
  folly::Function<int()> f1{[] { return 42; }};
  folly::Function<int()> f2;
  f2 = std::move(f1);
  int x = f2();
}
