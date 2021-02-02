/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// _AUTOMATICALLY_GENERATED_

package codetoanalyze.java.infer;

import javax.annotation.Nullable;

public class NeverNullSource {

  @Nullable T t;

  T get() {
    return t == null ? null : t;
  }
}
