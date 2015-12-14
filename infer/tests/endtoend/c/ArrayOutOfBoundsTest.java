/*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package endtoend.c;

import static org.hamcrest.MatcherAssert.assertThat;
import static utils.matchers.ResultContainsExactly.containsExactly;

import org.junit.BeforeClass;
import org.junit.Test;

import java.io.IOException;

import utils.InferException;
import utils.InferResults;

public class ArrayOutOfBoundsTest {

  public static final String SOURCE_FILE =
      "arithmetic/array_out_of_bounds.c";

  public static final String ARRAY_OUT_OF_BOUNDS_L1 =
      "ARRAY_OUT_OF_BOUNDS_L1";

  private static InferResults inferResults;

  @BeforeClass
  public static void runInfer() throws InterruptedException, IOException {
    inferResults = InferResults.loadCInferResults(
        ArrayOutOfBoundsTest.class,
        SOURCE_FILE
    );
  }

  @Test
  public void whenInferRunsOnBoundErrorThenArrayOutOfBoundsIsFound()
      throws InterruptedException, IOException, InferException {
    String[] procedures = {"bound_error", "bound_error_nested"};
    assertThat(
        "Results should contain array bound errors",
        inferResults,
        containsExactly(
            ARRAY_OUT_OF_BOUNDS_L1,
            SOURCE_FILE,
            procedures
        )
    );
  }

}
