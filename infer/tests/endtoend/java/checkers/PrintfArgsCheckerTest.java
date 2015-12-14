/*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package endtoend.java.checkers;

import static org.hamcrest.MatcherAssert.assertThat;
import static utils.matchers.ResultContainsExactly.containsExactly;

import org.junit.BeforeClass;
import org.junit.Test;

import java.io.IOException;

import utils.InferException;
import utils.InferResults;

public class PrintfArgsCheckerTest {


  public static final String SOURCE_FILE =
      "infer/tests/codetoanalyze/java/checkers/PrintfArgsChecker.java";

  public static final String CHECKERS_PRINTF_ARGS = "CHECKERS_PRINTF_ARGS";

  private static InferResults inferResults;

  @BeforeClass
  public static void loadResults() throws InterruptedException, IOException {
    inferResults =
      InferResults.loadCheckersResults(PrintfArgsCheckerTest.class, SOURCE_FILE);
  }

  @Test
  public void matchErrors()
      throws IOException, InterruptedException, InferException {
    String[] methods = {
        "notSuppressed",
        "stringInsteadOfInteger",
        "wrongNumberOfArguments",
        "formatStringIsNotLiteral",
    };
    assertThat(
        "Results should contain " + CHECKERS_PRINTF_ARGS,
        inferResults,
        containsExactly(
            CHECKERS_PRINTF_ARGS,
            SOURCE_FILE,
            methods));
  }

}
