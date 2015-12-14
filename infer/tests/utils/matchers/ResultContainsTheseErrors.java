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

public class ResultContainsTheseErrors extends BaseMatcher<InferResults> {

  private List<ErrorPattern> patterns;

  private ResultContainsTheseErrors(List<ErrorPattern> patterns) {
    this.patterns = patterns;
  }

  private boolean patternFound(ErrorPattern pattern, InferResults results) {
    for (InferError error : results.getErrors()) {
      if (pattern.match(error)) {
        return true;
      }
    }
    return false;
  }

  @Override
  public boolean matches(Object o) {
    InferResults results = (InferResults) o;
    for (ErrorPattern pattern : patterns) {
      if (!patternFound(pattern, results)) {
        return false;
      }
    }
    return true;
  }

  @Override
  public void describeTo(Description description) {
    String errorsString = "";
    for (ErrorPattern pattern : patterns) {
      errorsString += "\t" + pattern.toString() + "\n";
    }
    description.appendText("Expecting these errors: \n" + errorsString);
  }

  @Override
  public void describeMismatch(Object item, Description description) {

    InferResults results = (InferResults) item;
    String expectedErrors = "";

    for (ErrorPattern pattern : patterns) {
      if (!patternFound(pattern, results)) {
        expectedErrors += "\t" + pattern.toString() + "\n";
      }
    }
    description.appendText(
        "The following errors were not reported: \n" + expectedErrors + "\n" + "but found: " +
        results.toString() + "\n");
  }

  public static <T> Matcher<InferResults> contains(List<ErrorPattern> patterns) {
    return new ResultContainsTheseErrors(patterns);
  }

  public static <T> Matcher<InferResults> contains(String type, String file, String[] methods) {
    ArrayList<ErrorPattern> patterns = new ArrayList<>();
    for (String method : methods) {
      patterns.add(new ErrorPattern(type, file, method));
    }
    return new ResultContainsTheseErrors(patterns);
  }

}
