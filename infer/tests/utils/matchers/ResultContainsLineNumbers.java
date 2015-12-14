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

import java.util.Arrays;
import java.util.Vector;

import utils.InferError;
import utils.InferResults;

public class ResultContainsLineNumbers extends BaseMatcher<InferResults> {

  int[] lines;

  public ResultContainsLineNumbers(int[] lines) {
    this.lines = lines;
  }

  @Override
  public boolean matches(Object o) {
    InferResults results = (InferResults) o;
    boolean allContained = true;
    for (int line : lines) {
      boolean isContained = false;
      for (InferError error : results.getErrors()) {
        isContained = isContained || line == error.getErrorLine();
      }
      allContained = allContained && isContained;
    }
    return allContained;
  }

  @Override
  public void describeTo(Description description) {
    description.appendText("Correct line numbers in the error report.");
  }

  @Override
  public void describeMismatch(Object item, Description description) {
    InferResults results = (InferResults) item;
    String linesString = Arrays.toString(lines);
    String reportedLinesString = Arrays.toString(findLineNumbersInReport(results));
    description.appendText(
        "Infer did not report an error in the following lines: " + linesString +
            ". \n \t  Reported lines are: " + reportedLinesString +
            "\n" + results.inferCmdToString());
  }

  public static <T> Matcher<InferResults> containsLines(int[] lines) {
    return new ResultContainsLineNumbers(lines);
  }

  private int[] findLineNumbersInReport(InferResults results) {
    Vector<InferError> errors = results.getErrors();
    int nErrors = errors.size();
    int[] lines = new int[nErrors];
    for (int i = 0; i < nErrors; i++) {
      lines[i] = errors.get(i).getErrorLine();
    }
    return lines;
  }
}
