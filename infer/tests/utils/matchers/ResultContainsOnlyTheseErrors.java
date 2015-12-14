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

import java.util.ArrayList;
import java.util.List;

import utils.InferError;
import utils.InferResults;

public class ResultContainsOnlyTheseErrors extends BaseMatcher<InferResults> {

  private List<ErrorPattern> patterns;

  public ResultContainsOnlyTheseErrors(List<ErrorPattern> patterns) {
    this.patterns = patterns;
  }

  private boolean mathTypeAndAnyMethod(InferError e) {
    for (ErrorPattern pattern : patterns) {
      if (pattern.match(e)) {
        return true;
      }
    }
    return false;
  }

  @Override
  public boolean matches(Object o) {
    InferResults results = (InferResults) o;
    for (InferError error : results.getErrors()) {
      if (!mathTypeAndAnyMethod(error)) {
        return false;
      }
    }
    return true;
  }

  @Override
  public void describeTo(Description description) {
    String errorsString = "";
    for (ErrorPattern pattern : patterns) {
      errorsString =
          errorsString + "\t" + pattern.toString() + "\n";
    }
    description.appendText("Only these errors: \n" + errorsString);
  }

  @Override
  public void describeMismatch(Object item, Description description) {
    InferResults results = (InferResults) item;
    String unexpectedErrors = "";

    for (InferError error : results.getErrors()) {
      if (!mathTypeAndAnyMethod(error)) {
        unexpectedErrors = unexpectedErrors + "\t" + error + "\n";
      }
    }
    description.appendText(
        "Infer reported the following unexpected errors: \n" +
            unexpectedErrors + "\n" + results.inferCmdToString());
  }

  public static <T> Matcher<InferResults> containsOnly(String type, String file, String[] methods) {
    ArrayList<ErrorPattern> patterns = new ArrayList<>();
    for (String method : methods) {
      patterns.add(new ErrorPattern(type, file, method));
    }
    return new ResultContainsOnlyTheseErrors(patterns);
  }

  public static <T> Matcher<InferResults> containsOnly(List<ErrorPattern> patterns) {
    return new ResultContainsOnlyTheseErrors(patterns);
  }

}
