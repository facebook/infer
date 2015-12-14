/*
 * Copyright (c) 2015 - present Facebook, Inc.
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

public class ExpensiveSubtypingTest {

  public static final String SOURCE_FILE =
    "infer/tests/codetoanalyze/java/checkers/ExpensiveSubtypingExample.java";

  public static final String EXPENSIVE_OVERRIDES_UNANNOTATED =
    "CHECKERS_EXPENSIVE_OVERRIDES_UNANNOTATED";

  private static InferResults inferResults;

  @BeforeClass
  public static void loadResults() throws InterruptedException, IOException {
    inferResults =
      InferResults.loadCheckersResults(ExpensiveSubtypingTest.class, SOURCE_FILE);
  }

  @Test
  public void matchErrors()
    throws IOException, InterruptedException, InferException {
    String[] methods = {
      "m3",
    };
    assertThat(
        "Results should contain " + EXPENSIVE_OVERRIDES_UNANNOTATED,
        inferResults,
        containsExactly(
            EXPENSIVE_OVERRIDES_UNANNOTATED,
            SOURCE_FILE,
            methods));
    }

}
