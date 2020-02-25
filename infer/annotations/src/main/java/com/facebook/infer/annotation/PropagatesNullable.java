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
@Target({ElementType.PARAMETER})
/**
 * Annotation specifying method's contract. If a method's param is annotated
 * with @PropagaresNullable, it declares that the method will return {@code null} if and only if
 * this param is {@code null}.
 *
 * <p>Calls to @PropagatesNullable-annotated methods are treated by Nullsafe typechecker
 * accordingly. If param is non-nullable in the callsite, Nullsafe will assume the method result is
 * not null either, and hence no assertion or check will be needed.
 *
 * <p>In the following example, annotating the param with @PropagatesNullable allows to simplify
 * usage of the method.
 *
 * <pre>
 * public static String capitalize(@PropagatesNullable String input) {
 *   if (input == null) {
 *     return null;
 *   }
 *   return input.toUpperCase();
 * }
 *
 * void exampleOfUsage(@Nullable String nullable, String nonnull) {
 *   capitalize(nullable).contains("A"); // <-- BAD: need a check
 *   // if capitalize() was not annotated as @PropagatesNullable,
 *   // assertNotNull(capitalize(nonnull)) would be required.
 *   capitalize(nonnull).contains("A");  // <-- OK: safe to dereference
 * }
 * </pre>
 *
 * <p>If several params are annotated as {@code @PropagatesNullable}, the method should return
 * {@code null} if and only if any of those params is {@code null}.
 *
 * <p>See also {@code @TrueOnNull} and {@code @FalseOnNull} annotations.
 */
public @interface PropagatesNullable {}
