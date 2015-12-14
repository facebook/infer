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

import com.google.common.collect.ImmutableList;

import org.junit.BeforeClass;
import org.junit.ClassRule;
import org.junit.Test;

import java.io.IOException;

import utils.DebuggableTemporaryFolder;
import utils.InferException;
import utils.InferResults;
import utils.InferRunner;

public class InitListExprTest {

  public static final String SOURCE_FILE =
      "initialization/initlistexpr.c";

  public static final String DIVIDE_BY_ZERO = "DIVIDE_BY_ZERO";

  private static InferResults inferResults;

  @BeforeClass
  public static void loadResults() throws InterruptedException, IOException {
    inferResults = InferResults.loadCInferResults(InitListExprTest.class, SOURCE_FILE);
  }

  @Test
  public void whenInferRunsOnInitListExprThenDivideByZeroIsFound()
      throws InterruptedException, IOException, InferException {
    String[] methods = {
      "init_divide_by_zero"
    };
    assertThat(
        "Results should contain " + DIVIDE_BY_ZERO,
        inferResults,
        containsExactly(
            DIVIDE_BY_ZERO,
            SOURCE_FILE,
            methods
        )
    );
  }

}
