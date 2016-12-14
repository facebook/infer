/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

// have to pretend we're in the same package to access protected constructor
package com.google.common.collect;

import com.google.common.collect.ImmutableList;

abstract public class MyImmutableList<T> extends ImmutableList<T> {
  private Object mFld;

  public void writeFld() {
    mFld = new Object();
  }

}
