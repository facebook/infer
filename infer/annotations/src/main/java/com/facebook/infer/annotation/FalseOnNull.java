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

@Retention(RetentionPolicy.CLASS)
@Target({ElementType.METHOD})
/**
 * Annotation specifying method's contract. @FalseOnNull declares that a boolean method will always
 * return {@code false} when any of its arguments is {@code null}.
 *
 * <p>Calls to @FalseOnNull-annotated methods are treated by Nullsafe typechecker accordingly. This
 * allows the caller's code become more elegant without need of extra assertions.
 *
 * <p>In the following example, annotating a method with @FalseOnNull simplifies its usage.
 *
 * <pre>
 * @FalseOnNull
 * public static hasNotification(@Nullable MyObject object) {
 *  if (object == null) {
 *    return false;
 *  }
 *  // ... actual logic
 * }
 *
 * void exampleOfUsage(@Nullable MyObject obj) {
 *   if (hasNotification(obj)) {
 *     // Nullsafe knows obj can not be null at this point, so it can be safely dereferenced.
 *     // If hasNotification() was not annotated as @FalseOnNull, assertNotNull(obj)
 *     // would be required.
 *     obj.doSomething();
 *   }
 * }
 * </pre>
 *
 * <p>See also {@code @TrueOnNull} and {@code @PropagatesNullable} annotations.
 */
public @interface FalseOnNull {}
