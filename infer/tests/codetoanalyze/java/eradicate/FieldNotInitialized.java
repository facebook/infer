/*
* Copyright (c) 2013 - present Facebook, Inc.
* All rights reserved.
*
* This source code is licensed under the BSD style license found in the
* LICENSE file in the root directory of this source tree. An additional grant
* of patent rights can be found in the PATENTS file in the same directory.
*/

package codetoanalyze.java.eradicate;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

public class FieldNotInitialized {

  @Nullable
  String x;
  String y;
  @Nonnull
  String z; // Means: assume it's initialized to nonnull value somewhere else.

  FieldNotInitialized() {
  }
}
