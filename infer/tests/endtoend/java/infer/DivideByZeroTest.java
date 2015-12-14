/*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package endtoend.java.infer;

import static org.hamcrest.MatcherAssert.assertThat;
import static utils.matchers.ResultContainsErrorInMethod.contains;
import static utils.matchers.ResultContainsLineNumbers.containsLines;
import static utils.matchers.ResultContainsOnlyTheseErrors.containsOnly;

import org.junit.BeforeClass;
import org.junit.Test;

import java.io.IOException;

import utils.InferException;
import utils.InferResults;

public class DivideByZeroTest {

  public static final String DivideByZero =
      "infer/tests/codetoanalyze/java/infer/DivideByZero.java";

  public static final String DIVIDE_BY_ZERO = "DIVIDE_BY_ZERO";

  private static InferResults inferResults;

  @BeforeClass
  public static void loadResults() throws InterruptedException, IOException {
    inferResults = InferResults.loadInferResults(DivideByZeroTest.class, DivideByZero);
  }

  @Test
  public void whenInferRunsOnDivideByZeroLocalThenDivideByZeroErrorFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should contain divide by zero error.", inferResults,
        contains(
            DIVIDE_BY_ZERO,
            DivideByZero,
            "divByZeroLocal"
        )
    );
  }

  @Test
  public void whenInferRunsOnCallDivideByZeroInterProcThenDivideByZeroErrorFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should contain divide by zero error.",
        inferResults,
        contains(
            DIVIDE_BY_ZERO,
            DivideByZero,
            "callDivideByZeroInterProc"
        )
    );
  }

  @Test
  public void whenInferRunsOnDivideByZeroWithStaticFieldThenDivideByZeroErrorFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should contain divide by zero error.",
        inferResults,
        contains(
            DIVIDE_BY_ZERO,
            DivideByZero,
            "divideByZeroWithStaticField"
        )
    );
  }

  @Test
  public void whenInferRunsOnDivideByZeroThenTheLineNumbersAreReportedCorr()
      throws InterruptedException, IOException, InferException {
    int[] lines = {28};
    assertThat(
        "Result should contain correct line numbers.",
        inferResults,
        containsLines(lines));
  }

  @Test
  public void whenInferRunsOnDivideByZeroThenOnlyTheExpectedErrorsAreFound()
      throws InterruptedException, IOException, InferException {
    String[] expectedMethods = {
        "divByZeroLocal",
        "callDivideByZeroInterProc",
        "divideByZeroWithStaticField"
    };
    assertThat(
        "No unexpected errors should be found", inferResults,
        containsOnly(
            DIVIDE_BY_ZERO,
            DivideByZero,
            expectedMethods));
  }

}
