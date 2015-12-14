/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package utils.matchers;

import org.hamcrest.BaseMatcher;
import org.hamcrest.Description;
import org.hamcrest.Matcher;

import utils.InferError;
import utils.InferResults;

public class ResultContainsNumberOfErrorsInMethod extends BaseMatcher<InferResults> {

  private final ErrorPattern pattern;
  private final int numberOfErrors;
  int actualNumberErrors;

  public ResultContainsNumberOfErrorsInMethod(
      String type,
      String file,
      String method,
      int numberOfErrors) {
    this.pattern = new ErrorPattern(type, file, method);
    this.numberOfErrors = numberOfErrors;
    this.actualNumberErrors = 0;
  }

  @Override
  public boolean matches(Object o) {
    InferResults results = (InferResults) o;
    for (InferError error : results.getErrors()) {
      if (pattern.match(error)) {
        actualNumberErrors++;
      }
    }
    return (actualNumberErrors == numberOfErrors);
  }

  @Override
  public void describeTo(Description description) {
    description.appendText(
        numberOfErrors + " " + pattern.getErrorType()
            + " errors in file: " + pattern.getErrorFile()
            + ", method: " + pattern.getErrorMethod());
  }

  @Override
  public void describeMismatch(Object item, Description description) {
    InferResults results = (InferResults) item;
    description.appendText(
        actualNumberErrors + " " + pattern.getErrorType() +
            " errors found in " + pattern.getErrorMethod() + " in the results of Infer." +
            "\n" + results.inferCmdToString());
  }

  public static <T> Matcher<InferResults> containsNumberOfErrors(
      String type,
      String file,
      String method,
      int numberOfErrors) {
    return new ResultContainsNumberOfErrorsInMethod(type, file, method, numberOfErrors);
  }

}
