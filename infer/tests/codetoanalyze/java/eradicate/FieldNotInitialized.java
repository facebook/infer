/*
* Copyright (c) 2013- Facebook.
* All rights reserved.
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
