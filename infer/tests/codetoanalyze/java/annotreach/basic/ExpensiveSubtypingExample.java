/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.checkers;

import com.facebook.infer.annotation.Expensive;

public class ExpensiveSubtypingExample extends ExpensiveInterfaceExample.C {

  @Expensive
  public void m3() {}

  public void m4() {}
}
