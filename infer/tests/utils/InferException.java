/*
* Copyright (c) 2013- Facebook.
* All rights reserved.
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
