/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
class __attribute__((capability("mutex"))) Mutex {
 public:
  const Mutex &operator!() const { return *this; }
};

class C {
  Mutex mu;
  Mutex *mu_ptr;
  int x __attribute__((guarded_by(mu)));

  int get() __attribute__((requires_capability(mu))) { return x; }
  int get_shared() __attribute__((requires_shared_capability(mu, !mu_ptr)));
  void set(int v) __attribute__((requires_capability(!mu)));
};
