// Copyright (c) 2015-Present Facebook. All rights reserved.

package endtoend.java.comparison;

import static org.hamcrest.MatcherAssert.assertThat;
import static utils.matchers.ResultContainsExactly.containsExactly;

import org.junit.BeforeClass;
import org.junit.Test;

import java.io.IOException;

import utils.InferException;
import utils.InferResults;

public class ArrayIndexOutOfBoundsExceptionTest {

  public static final String SOURCE_FILE =
      "infer/tests/codetoanalyze/java/infer/ArrayOutOfBounds.java";

  public static final String ARRAY_OUT_OF_BOUNDS = "java.lang.ArrayIndexOutOfBoundsException";

  private static InferResults inferResults;

  @BeforeClass
  public static void loadResults() throws InterruptedException, IOException {
    inferResults = InferResults.loadTracingComparisonResults(
        ArrayIndexOutOfBoundsExceptionTest.class,
        SOURCE_FILE);
  }

  @Test
  public void whenInferRunsOnArrayOutOfBoundsThenErrorIsFound()
      throws IOException, InterruptedException, InferException {
    String[] methods = {
        "arrayOutOfBounds",
    };
    assertThat(
        "Results should contain out of bounds error.", inferResults,
        containsExactly(
            ARRAY_OUT_OF_BOUNDS,
            SOURCE_FILE,
            methods
        )
    );
  }

}
