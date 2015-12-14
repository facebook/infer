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

public class ResultContainsZeroErrorsInMethod extends BaseMatcher<InferResults> {

  String fileName;
  String methodName;
  int actualNumberErrors;

  public ResultContainsZeroErrorsInMethod(String fileName, String methodName) {
    this.fileName = fileName;
    this.methodName = methodName;
  }

  @Override
  public boolean matches(Object o) {
    InferResults results = (InferResults) o;
    for (InferError error : results.getErrors()) {
      if (fileName.equals(error.getErrorFile()) &&
          methodName.equals(error.getErrorMethod())) {
        actualNumberErrors++;
      }
    }
    return actualNumberErrors == 0;
  }

  @Override
  public void describeTo(Description description) {
    description.appendText(actualNumberErrors + " errors found in method." + methodName);
  }

  @Override
  public void describeMismatch(Object item, Description description) {
    InferResults results = (InferResults) item;
    description.appendText(
        actualNumberErrors + " errors found in " + methodName +
            " in the results of Infer, but expected 0." + "\n" + results.inferCmdToString());
  }

  public static <T> Matcher<InferResults> containsZeroErrors(String fileName, String methodName) {
    return new ResultContainsZeroErrorsInMethod(fileName, methodName);
  }

}
