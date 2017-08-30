/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
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
