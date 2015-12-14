/*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package utils;


public class InferException extends RuntimeException {

  public InferException(String message) {
    super(message);
  }

  @Override
  public String toString() {
    return "";
  }

  @Override
  public synchronized Throwable fillInStackTrace() {
    return this;
  }
}
