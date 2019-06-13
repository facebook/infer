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
 * View can be annotated with @SuppressViewNullability to silence warnings when a view is set to
 * null in a destructor, and created in an initializer.
 */
@Retention(RetentionPolicy.CLASS)
@Target(ElementType.FIELD)
public @interface SuppressViewNullability {}
