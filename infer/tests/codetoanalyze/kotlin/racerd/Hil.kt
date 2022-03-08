/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

class Hil(public val o: Object) {
  fun crashInfer(nums: List<Long>) {
    val foldedNums = nums.fold(1) { _, _ -> 2 }

    val checked = checkNotNull(o) { "Boom" }

    val v = foldedNums.toLong()
  }
}
