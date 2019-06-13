/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package crossmodule.module1;

import genrule.annotations.Nullable;

public class Class1 {

  public static String returnsNull() {
    return null;
  }

  @Nullable public Object nullableField;
}
