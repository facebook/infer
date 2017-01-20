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

/** assume that a method is thread-safe without actually checking it (as opposed to @ThreadSafe,
    which does check). useful for suppressing warnings involving benign races */

@Target({ ElementType.METHOD })
@Retention(RetentionPolicy.CLASS)
public @interface AssumeThreadSafe {
  String because(); /** describe why the thread-safety assumption is reasonable */
}
