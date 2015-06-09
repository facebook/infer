// Copyright 2004-present Facebook. All Rights Reserved.

package com.facebook.infer.annotation;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * A method annotated with @Initializer should always be be called before the object is used.
 * Users of the class and static checkers must enforce, and can rely on, this invariant.
 * Examples include methods called indirectly by the constructor, protocols of init-then-use
 * where some values are initialized after construction but before the first use,
 * and builder classes where an object initialization must complete before build() is called.
 */
@Retention(RetentionPolicy.CLASS)
@Target({ElementType.TYPE, ElementType.FIELD, ElementType.CONSTRUCTOR, ElementType.METHOD})
public @interface Initializer {}
