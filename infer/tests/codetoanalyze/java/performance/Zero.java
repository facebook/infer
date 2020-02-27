/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
import com.google.common.base.Preconditions;

class Zero {

  // cost: 1
  void unit_cost() {};

  public void infeasible_path_zero() {
    Preconditions.checkState(false); // pruned to bottom
  }

  // we can't handle doubles properly in Inferbo
  public void double_prune_FN(double fpp) {
    Preconditions.checkArgument(fpp > 0.0 && fpp < 0.0);
  }
}
