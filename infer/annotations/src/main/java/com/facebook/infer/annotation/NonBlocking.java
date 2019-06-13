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
@Target({ElementType.CONSTRUCTOR, ElementType.METHOD, ElementType.TYPE})

// Signal to the starvation checker that the method (or all the methods of the class,
// if at class level) does not perform any potentially blocking operations.  Can be used to
// effectively filter out all method calls which Infer may consider blocking.  This means that
// not only Infer will not warn on any starvation issues in the method, but will also not warn on
// any of the callers of this method.
public @interface NonBlocking {}
