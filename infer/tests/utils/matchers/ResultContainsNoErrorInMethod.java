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

public class ResultContainsNoErrorInMethod extends BaseMatcher<InferResults> {

  ErrorPattern pattern;

  public ResultContainsNoErrorInMethod(String type, String file, String method) {
    pattern = new ErrorPattern(type, file, method);
  }

  @Override
  public boolean matches(Object o) {
    InferResults results = (InferResults) o;
    for (InferError foundError : results.getErrors()) {
      if (pattern.match(foundError)) {
        return false;
      }
    }
    return true;
  }

  @Override
  public void describeTo(Description description) {
    description.appendText(
        "No " + pattern.getErrorType() + " error in " + pattern.getErrorMethod());
  }

  @Override
  public void describeMismatch(Object item, Description description) {
    InferResults results = (InferResults) item;
    description.appendText(
        pattern.getErrorType() + " error found in " +
            pattern.getErrorMethod() + " in the results of Infer.\n" +
            results.inferCmdToString());
  }

  public static <T> Matcher<InferResults> doesNotContain(String type, String file, String method) {
    return new ResultContainsNoErrorInMethod(type, file, method);
  }

}
