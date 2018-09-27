/*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package threadsafety_traces.module3;

import threadsafety_traces.module2.Class2;

@interface ThreadSafe {}

@ThreadSafe
public class Class3 {

  /**
   * this will produce a truncated trace that should bottom out in Class1.method, but will stop
   * short due to limitations in our Buck integration. test that we don't report a truncated trace
   * in this situation.
   */
  public void callClass2() {
    Class2.method();
  }
}
