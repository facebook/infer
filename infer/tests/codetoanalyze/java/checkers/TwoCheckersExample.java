/*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.checkers;

import com.google.common.collect.ImmutableList;

import com.facebook.infer.annotation.Expensive;
import com.facebook.infer.annotation.PerformanceCritical;

import java.util.List;

public class TwoCheckersExample {

  @Expensive
  static List shouldRaiseImmutableCastError() {
    return ImmutableList.of();
  }

  @PerformanceCritical
  static List shouldRaisePerformanceCriticalError() {
    return shouldRaiseImmutableCastError();
  }

}
