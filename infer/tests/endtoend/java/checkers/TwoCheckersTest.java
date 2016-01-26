/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package endtoend.java.checkers;

import static org.hamcrest.MatcherAssert.assertThat;
import static utils.matchers.ResultContainsErrorInMethod.contains;

import org.junit.BeforeClass;
import org.junit.Test;

import java.io.IOException;

import utils.InferException;
import utils.InferResults;

public class TwoCheckersTest {

  public static final String SOURCE_FILE =
    "infer/tests/codetoanalyze/java/checkers/TwoCheckersExample.java";

  public static final String CALLS_EXPENSIVE_METHOD =
    "CHECKERS_CALLS_EXPENSIVE_METHOD";

  public static final String IMMUTABLE_CAST =
    "CHECKERS_IMMUTABLE_CAST";

  private static InferResults inferResults;

  @BeforeClass
  public static void loadResults() throws InterruptedException, IOException {
    inferResults =
      InferResults.loadCheckersResults(TwoCheckersTest.class, SOURCE_FILE);
  }

  @Test
  public void immutableCastErrorIsFound ()
      throws IOException, InterruptedException, InferException {
    assertThat("Results should contain " + IMMUTABLE_CAST,
               inferResults,
               contains(IMMUTABLE_CAST,
                        SOURCE_FILE,
                        "shouldRaiseImmutableCastError"));
  }

  @Test
  public void PerformanceCriticalErrorIsFound ()
    throws IOException, InterruptedException, InferException {
    assertThat("Results should contain " + CALLS_EXPENSIVE_METHOD,
               inferResults,
               contains(CALLS_EXPENSIVE_METHOD,
                        SOURCE_FILE,
                        "shouldRaisePerformanceCriticalError"));
  }

}
