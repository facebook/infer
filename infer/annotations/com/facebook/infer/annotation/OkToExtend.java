// Copyright 2004-present Facebook. All Rights Reserved.

package com.facebook.infer.annotation;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;

/**
 * Marks a class as one that is expected to be extended.
 *
 * This annotation is meant to counter common misuses of subclassing. Annotate your class with this
 * only if it was built with the purpose of being extended.
 *
 * Avoid adding this to classes that have existed for a long time without needing it.
 */
@Retention(RetentionPolicy.SOURCE)
public @interface OkToExtend {

}
