/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.checkers;

import com.facebook.infer.annotation.IgnoreAllocations;
import com.facebook.infer.annotation.NoAllocation;

public class NoAllocationExample {

  @NoAllocation
  void directlyAllocatingMethod() {
    new Object();
  }

  void allocates() {
    new Object();
  }

  @NoAllocation
  void indirectlyAllocatingMethod() {
    allocates();
  }

  void doesNotAllocate() {
    // does noting
  }

  @NoAllocation
  void notAllocatingMethod() {
    doesNotAllocate();
  }

  void allocatingIsFine() {
    new Object();
  }

  @NoAllocation
  void throwsException() {
    throw new RuntimeException();
  }

  @NoAllocation
  void creatingExceptionIsFine() {
    throwsException();
  }

  @NoAllocation
  void thowingAThrowableIsFine() {
    throw new AssertionError();
  }

  @IgnoreAllocations
  void acceptableAllocation() {
    new Object();
  }

  @NoAllocation
  void onlyAllocatesInAcceptableWay() {
    acceptableAllocation();
  }
}
