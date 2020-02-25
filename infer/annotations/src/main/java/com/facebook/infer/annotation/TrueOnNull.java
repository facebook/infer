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
 * Annotation specifying method's contract. @TrueOnNull declares that a boolean method will always
 * return {@code true} when any of its arguments is {@code null}.
 *
 * <p>Calls to @TrueOnNull-annotated methods are treated by Nullsafe typechecker accordingly. This
 * allows the caller's code become more elegant without need of extra assertions.
 *
 * <p>In the following example, annotating a method with @TrueOnNull simplifies its usage.
 *
 * <pre>
 * @TrueOnNull
 * public static boolean isStringEmpty(@Nullable String str) {
 *   return str == null || str.length() == 0;
 * }
 *
 * void exampleOfUsage(@Nullable String myString) {
 *   if (!isStringEmpty(myString)) {
 *     // Nullsafe knows myString can not be null at this point, so it can be safely dereferenced.
 *     // If isStringEmpty() was not annotated as @TrueOnNull, assertNotNull(myString)
 *     // would be required.
 *     myString.toUpperCase();
 *   }
 * }
 * </pre>
 *
 * <p>See also {@code @FalseOnNull} and {@code @PropagatesNullable} annotations.
 */
public @interface TrueOnNull {}
