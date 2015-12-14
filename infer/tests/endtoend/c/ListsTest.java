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

public class ListsTest {

  public static final String SOURCE_FILE = "lists/lists.c";

  public static final String DIVIDE_BY_ZERO = "DIVIDE_BY_ZERO";

  private static InferResults inferResults;

  @BeforeClass
  public static void runInfer() throws InterruptedException, IOException {
    inferResults = InferResults.loadCInferResults(DivideByZeroTest.class, SOURCE_FILE);
  }

  @Test
  public void whenInferRunsOnDivideByZeroThenDivideByZeroIsFound()
      throws InterruptedException, IOException, InferException {
    String[] procedures = {"main"};
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
