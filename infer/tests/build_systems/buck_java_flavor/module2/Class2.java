/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package buck_java_flavor.module2;

import buck_java_flavor.annotations.ThreadSafe;
import buck_java_flavor.module1.Class1;

@ThreadSafe
public class Class2 {
  Class1 c;

  void set(int x) {
    c.set(x);
  }

  int get() {
    return c.get();
  }
}
