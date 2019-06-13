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
 * A class field, or method return/parameter type, of Optional type is annotated @Present to
 * indicate that its value cannot be absent. Users of the method/field and static checkers must
 * enforce, and can rely on, this invariant.
 */
@Retention(RetentionPolicy.CLASS)
@Target({
  ElementType.TYPE,
  ElementType.FIELD,
  ElementType.CONSTRUCTOR,
  ElementType.METHOD,
  ElementType.PARAMETER
})
public @interface Present {}
