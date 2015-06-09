/*
 * Copyright (c) 2013- Facebook.
 * All rights reserved.
 */

package endtoend.c;

import static org.hamcrest.MatcherAssert.assertThat;
import static utils.matchers.ResultContainsExactly.containsExactly;

import org.junit.BeforeClass;
import org.junit.Test;

import java.io.IOException;

import utils.InferException;
import utils.InferResults;

public class DivideByZeroTest {

  public static final String SOURCE_FILE =
      "arithmetic/divide_by_zero.c";

  public static final String DIVIDE_BY_ZERO = "DIVIDE_BY_ZERO";

  private static InferResults inferResults;

  @BeforeClass
  public static void runInfer() throws InterruptedException, IOException {
    inferResults = InferResults.loadCInferResults(DivideByZeroTest.class, SOURCE_FILE);
  }

  @Test
  public void whenInferRunsOnDivideByZeroThenDivideByZeroIsFound()
      throws InterruptedException, IOException, InferException {
    String[] procedures = {"divide_by_zero"};
    assertThat(
        "Results should contain divide by zero error",
        inferResults,
        containsExactly(
            DIVIDE_BY_ZERO,
            SOURCE_FILE,
            procedures
        )
    );
  }


}
