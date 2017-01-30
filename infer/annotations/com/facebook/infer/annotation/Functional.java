/*
 * Copyright (c) 2004 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package com.facebook.infer.annotation;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/** Annotation for methods that always return the same value. The annotation is inherited by
  * overrides of methods.
  *
  * This annotation is used to suppress benign race warnings on fields assigned to methods annotated
  * with @Functional in the thread-safety analysis. For example:
  *
  * T mField;
  * @Functional T someMethod();
  * public void getField() {
  *   if (mField == null) {
  *     mField = someMethod();
  *  }
  *  return mField;
  * }
  *
  * Normally, we'd report that the access to mField is unsafe, but we won't report here because of
  * the @Functional annotation.
  *
  * If the return value of the annotated function is a double or long, the annotation will be
  * ignored because writes to doubles/longs are not guaranteed to be atomic. That is, if type T was
  * `long` in the example above, the write-write race on mField would no longer be benign.
*/

@Target({ElementType.METHOD})
@Retention(RetentionPolicy.CLASS)
public @interface Functional {}
