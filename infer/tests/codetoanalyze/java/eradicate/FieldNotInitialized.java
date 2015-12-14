/*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package codetoanalyze.java.eradicate;


import android.support.annotation.NonNull;
import android.widget.EditText;

import butterknife.Bind;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import javax.inject.Inject;

public class FieldNotInitialized {

  String a;

  @Nullable String b;

  @Nonnull String c; // Means: assume it will be initialized to a nonnull value somewhere else.

  @Inject String d; // Means: assume it will be initialized via dependency injection

  @NonNull String e;

  @Bind(42) EditText f;

  //  Eradicate should only report one initialization error
  FieldNotInitialized() {}

}
