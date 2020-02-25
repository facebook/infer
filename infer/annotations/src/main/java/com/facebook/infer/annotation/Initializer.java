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
@Target({ElementType.TYPE, ElementType.FIELD, ElementType.CONSTRUCTOR, ElementType.METHOD})
/**
 * A method annotated with @Initializer is expected to always be invoked before the object is used.
 * Nullsafe typechecker respects this annotation when checking field initialization: if a field is
 * assigned a (non-nullable) value in @Initializer-annotated method, it is considered initialized,
 * and hence does not require @Nullable annotation.
 *
 * <p>Methods annotated as @Initializer should not be private. If the actual initialization is
 * happening in a private helper method, then the public method that calls this helper method should
 * be annotated.
 *
 * <p>In this example, only field2 will be reported as not initialized:
 *
 * <pre>
 * class Example {
 *   private String field1;
 *   private String field2;
 *   // Will be initialized in finishCreation().
 *   // (It is a good idea to specify that in comments in your real code!)
 *   private String field3;
 *
 *   public Example() {
 *     field1 = "OK: initialized in the constructor";
 *     // BAD: did not initialize field2!
 *     // (But OK not to initialize field3).
 *   }
 *
 *   // This should be called before the object can be used.
 *   // It is a good idea to always document @Initializer methods for your client).
 *   @Initializer
 *   public void finishCreation() {
 *     field3 = "OK: Nullsafe assumes this will be called after creation";
 *   }
 * }
 * </pre>
 *
 * <p>See also: {@code @Cleanup} annotation.
 */
public @interface Initializer {}
