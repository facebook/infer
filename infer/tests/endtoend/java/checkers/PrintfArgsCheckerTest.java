/*
* Copyright (c) 2013- Facebook.
* All rights reserved.
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
