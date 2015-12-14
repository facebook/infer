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
import utils.InferStats;

public class NumberOfProceduresAnalyzed extends BaseMatcher<InferStats> {

  private int expectedNumProcedures;
  private int actualNumProcedures;

  public NumberOfProceduresAnalyzed(int expectedNumProcedures) {
    this.expectedNumProcedures = expectedNumProcedures;
    this.actualNumProcedures = -1;
  }

  @Override
  public boolean matches(Object o) {
    InferStats stats = (InferStats) o;
    actualNumProcedures = stats.getNumProcedures();
    return actualNumProcedures == expectedNumProcedures;
  }

  @Override
  public void describeTo(Description description) {
    description.appendText(expectedNumProcedures + " procedures analyzed by Infer.");
  }

  @Override
  public void describeMismatch(Object item, Description description) {
    InferStats stats = (InferStats) item;
    description.appendText("found " + actualNumProcedures + " procedures analyzed by Infer.");
  }

  public static <T> Matcher<InferStats> numberOfProceduresAnalyzed(
      int expectedNumProcedures) {
    return new NumberOfProceduresAnalyzed(expectedNumProcedures);
  }

}
