/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package com.facebook.infer.annotation;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Similar to the {@literal @ThreadSafe} annotation from javax.concurrent.annotation, but can be
 * applied to methods. In addition, you can ask Infer to assume thread-safety rather than checking
 * it by using {@literal @ThreadSafe(enableChecks = false)}.
 */
@Target({ElementType.CONSTRUCTOR, ElementType.METHOD, ElementType.TYPE})
@Retention(RetentionPolicy.CLASS)
public @interface ThreadSafe {
  boolean enableChecks() default true;
}
