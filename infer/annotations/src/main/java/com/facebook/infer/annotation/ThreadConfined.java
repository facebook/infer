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
 * This annotation tells the thread-safety analysis to assume that mutations in the annotated
 * class/field/method are confined to the given thread name. For the thread name, you can either use
 * the default constants UI/ANY or add your own.
 */
@Target({ElementType.TYPE, ElementType.FIELD, ElementType.METHOD})
@Retention(RetentionPolicy.CLASS)
public @interface ThreadConfined {
  String value();
  /** the thread that the mutations should be confined to */
  public static String UI = "UI";
  /** confined to the UI thread */
  public static String ANY = "ANY";
  /** confined to any thread (but only that thread!) */
}
