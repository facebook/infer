/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package threadsafety_traces.module3;

import threadsafety_traces.module2.Class2;

@interface ThreadSafe {
}

@ThreadSafe
public class Class3 {

  /** this will produce a truncated trace that should bottom out in Class1.method, but will stop
      short due to limitations in our Buck integration. test that we don't report a truncated
      trace in this situation. */
  public void callClass2() {
    Class2.method();
  }

}
