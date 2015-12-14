/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

// _AUTOMATICALLY_GENERATED_

package codetoanalyze.java.infer;

import javax.annotation.Nullable;

public class NeverNullSource {

  @Nullable
  T t;

  T get() {
    return t == null ? null : t;
  }

}
