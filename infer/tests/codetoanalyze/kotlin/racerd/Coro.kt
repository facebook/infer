/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

class Coro(public val o: Object) {
  val fibonacciSeq = sequence {
    var a = 0
    var b = 1

    yield(1)

    while (true) {
      yield(a + b)

      val tmp = a + b

      synchronized(this) { a = b }
      b = tmp
    }
  }
}
