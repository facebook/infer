/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

import com.google.common.annotations.VisibleForTesting
import javax.annotation.concurrent.GuardedBy

class GuardedByTests {
  @GuardedBy("this") private var unguardedBadField = 0

  fun unguardedBad() {
    unguardedBadField = 0
  }

  @GuardedBy("this") private var synchronizedField = 0

  @Synchronized
  fun synchronizedMethodOk() {
    synchronizedField = 0
  }

  fun synchronizedBlockOk() {
    synchronized(this) { synchronizedField = 0 }
  }

  @GuardedBy("this") private var unguardedReadField = 0

  fun unguardedReadOk(): Int {
    return unguardedReadField
  }

  @GuardedBy("this") private var privateUnguardedField = 0

  private fun privateUnguardedOk() {
    privateUnguardedField = 0
  }

  fun interprocUnguardedBad() {
    privateUnguardedOk()
  }

  @GuardedBy("this") private var privateInterprocSynchronizedField = 0

  private fun privateInterprocSynchronizedOk() {
    privateInterprocSynchronizedField = 0
  }

  @Synchronized
  fun interprocSynchronizedOk() {
    privateInterprocSynchronizedOk()
  }

  @GuardedBy("this") private var visibleForTestingField = 0

  @VisibleForTesting
  fun visibleForTestingOk() {
    visibleForTestingField = 0
  }
}
