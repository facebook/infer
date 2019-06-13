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
 * Tell the thread-safety analysis that this method transfers ownership of its return value to its
 * caller. Ownership means that the caller is allowed to both read and write the value outside of
 * synchronization. The annotated method should not retain any references to the value. This
 * annotation is trusted for now, but may be checked eventually.
 */
@Target(ElementType.METHOD)
@Retention(RetentionPolicy.CLASS)
public @interface ReturnsOwnership {}
