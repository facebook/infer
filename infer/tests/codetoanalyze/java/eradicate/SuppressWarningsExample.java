/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package codetoanalyze.java.eradicate;

import java.lang.SuppressWarnings;

import javax.annotation.Nullable;

public class SuppressWarningsExample {

  @SuppressWarnings("null")
  static class DoSuppress {

    @Nullable Object mFld;

    void doSuppress1(@Nullable Object o) {
      o.toString();
    }

    void doSuppress2() {
      mFld.toString();
    }
  }

  @SuppressWarnings("null")
  public void doSuppress(@Nullable Object o) {
    o.toString();
  }

  @SuppressWarnings("infer")
  public void doSuppressInferAnnot(@Nullable Object o) {
    o.toString();
  }

  @SuppressWarnings("unchecked")
  public void doNotSuppressWrongAnnot(@Nullable Object o) {
    o.toString();
  }

  public void doNotSuppressNoAnnot(@Nullable Object o) {
    o.toString();
  }

}
