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

public class ExpensiveCallTest {

  public static final String SOURCE_FILE =
    "infer/tests/codetoanalyze/java/checkers/ExpensiveCallExample.java";

  public static final String CALLS_EXPENSIVE_METHOD =
    "CHECKERS_CALLS_EXPENSIVE_METHOD";

  private static InferResults inferResults;

  @BeforeClass
  public static void loadResults() throws InterruptedException, IOException {
    inferResults =
      InferResults.loadCheckersResults(ExpensiveCallTest.class, SOURCE_FILE);
  }

  @Test
  public void matchErrors()
      throws IOException, InterruptedException, InferException {
    String[] methods = {
        "directlyCallingExpensiveMethod",
        "indirectlyCallingExpensiveMethod",
        "callingExpensiveMethodFromInterface",
        "longerCallStackToExpensive",
        "callsFindViewByIdFromView",
        "callsFindViewByIdFromActivity",
        "annotatedPerformanceCriticalInInterface",
        "performanceCriticalMethod1",
        "performanceCriticalMethod2",
        "performanceCriticalMethod3",
        // TODO: make subclassing work
        //"subclassPerformanceCriticalMethod1",
        //"subclassPerformanceCriticalMethod2",
        //"subclassPerformanceCriticalMethod3",
        "callMethodOnExpensiveClass"
    };
    assertThat(
        "Results should contain " + CALLS_EXPENSIVE_METHOD,
        inferResults,
        containsExactly(
            CALLS_EXPENSIVE_METHOD,
            SOURCE_FILE,
            methods));
  }

}
