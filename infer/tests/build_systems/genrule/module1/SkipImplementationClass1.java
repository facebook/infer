/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// _SHOULD_SKIP_IMPLEMENTATION_

package genrule.module1;

import genrule.annotations.Nullable;

public class SkipImplementationClass1 {

  public @Nullable Object annotatedNullable() {
    return new Object();
  }

  public Object notAnnotatedNullable() {
    return null;
  }
}
