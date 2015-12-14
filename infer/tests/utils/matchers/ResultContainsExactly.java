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

import utils.InferResults;

public class ResultContainsExactly extends BaseMatcher<InferResults> {

  Matcher<InferResults> containsAll;
  Matcher<InferResults> containsOnly;

  private ResultContainsExactly(List<ErrorPattern> patterns) {
    containsAll = ResultContainsTheseErrors.contains(patterns);
    containsOnly = ResultContainsOnlyTheseErrors.containsOnly(patterns);
  }

  @Override
  public boolean matches(Object o) {
    return containsAll.matches(o) && containsOnly.matches(o);
  }

  @Override
  public void describeTo(Description description) {
    containsAll.describeTo(description);
  }

  @Override
  public void describeMismatch(Object item, Description description) {
    if (!containsAll.matches(item)) {
      containsAll.describeMismatch(item, description);
    }
    if (!containsOnly.matches(item)) {
      containsOnly.describeMismatch(item, description);
    }
  }

  public static <T> Matcher<InferResults> containsExactly(List<ErrorPattern> patterns) {
    return new ResultContainsExactly(patterns);
  }

  public static <T> Matcher<InferResults> containsExactly(
      String type,
      String file,
      String[] methods) {
    ArrayList<ErrorPattern> patterns = new ArrayList<>();
    for (String method : methods) {
      patterns.add(new ErrorPattern(type, file, method));
    }
    return new ResultContainsExactly(patterns);
  }

}
