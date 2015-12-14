/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */


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
