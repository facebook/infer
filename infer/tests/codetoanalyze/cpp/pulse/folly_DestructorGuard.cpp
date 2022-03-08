/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

namespace folly {

class DelayedDestructionBase {};

class DelayedDestruction : public DelayedDestructionBase {
 public:
  virtual void destroy() { delete this; } // model ignores delete
};

} // namespace folly

class UsingDelayedDestruction : folly::DelayedDestruction {

  void double_delete_bad() {
    delete this;
    delete this;
  }

  // should realise [this] cannot be null to avoid FP latent (that can never be
  // manifested)
  // FP latent is filtered out
  void latent_double_delete_ok() {
    destroy(); // should not delete this double delete
    delete this;
  }
};
