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

public class ResultContainsErrorNoFilename extends BaseMatcher<InferResults> {

  private String errorType;
  private String errorMethod;

  public ResultContainsErrorNoFilename(String type, String method) {
    this.errorType = type;
    this.errorMethod = method;
  }

  @Override
  public boolean matches(Object o) {
    InferResults results = (InferResults) o;
    for (InferError foundError : results.getErrors()) {
      if (foundError.matchType(this.errorType)
          && foundError.matchMethod(this.errorMethod)) {
        return true;
      }
    }
    return false;
  }

  @Override
  public void describeTo(Description description) {
    description.appendText(
        this.errorType + " error in " + this.errorMethod);
  }

  @Override
  public void describeMismatch(Object item, Description description) {
    InferResults results = (InferResults) item;
    String resultsString = "";
    for (InferError error : results.getErrors()) {
      resultsString = resultsString + "\n\t" + error;
    }
    description.appendText(
        "Found errors: \n" + resultsString
            + "\nwith:\n" + results.inferCmdToString());
  }

  public static <T> Matcher<InferResults> contains(String type, String method) {
    return new ResultContainsErrorNoFilename(type, method);
  }

}
