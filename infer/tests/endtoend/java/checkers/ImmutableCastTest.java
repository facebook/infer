/*
 * Copyright (c) 2013 - present Facebook, Inc.
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

public class ImmutableCastTest {


  public static final String SOURCE_FILE =
      "infer/tests/codetoanalyze/java/checkers/ImmutableCast.java";

  public static final String IMMUTABLE_CAST = "CHECKERS_IMMUTABLE_CAST";

  private static InferResults inferResults;

  @BeforeClass
  public static void loadResults() throws InterruptedException, IOException {
    inferResults =
      InferResults.loadCheckersResults(ImmutableCastTest.class, SOURCE_FILE);
  }

  @Test
  public void matchErrors()
      throws IOException, InterruptedException, InferException {
    String[] methods = {
        "badCast",
        "badCastFromField",
    };
    assertThat(
        "Results should contain " + IMMUTABLE_CAST,
        inferResults,
        containsExactly(
            IMMUTABLE_CAST,
            SOURCE_FILE,
            methods));
  }

}
