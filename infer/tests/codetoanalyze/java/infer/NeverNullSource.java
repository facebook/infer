/*
* Copyright (c) 2015-present Facebook.
* All rights reserved.
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
