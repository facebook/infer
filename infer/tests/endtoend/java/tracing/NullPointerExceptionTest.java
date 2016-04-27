/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */


package endtoend.java.tracing;

import static org.hamcrest.MatcherAssert.assertThat;
import static utils.matchers.ResultContainsExactly.containsExactly;

import org.junit.BeforeClass;
import org.junit.Test;

import java.io.IOException;

import utils.InferException;
import utils.InferResults;

public class NullPointerExceptionTest {

  public static final String SOURCE_FILE =
      "infer/tests/codetoanalyze/java/tracing/NullPointerExceptionExample.java";

  public static final String NPE =
      "java.lang.NullPointerException";

  private static InferResults inferResults;

  @BeforeClass
  public static void loadResults() throws InterruptedException, IOException {
    inferResults = InferResults.loadTracingResults(
        NullPointerExceptionTest.class,
        SOURCE_FILE);
  }

  @Test
  public void matchErrors()
      throws IOException, InterruptedException, InferException {
    String[] methods = {
        "callDeref",
        "callLeadToNpe",
        "npeOnBothBranches",
    };
    assertThat(
        "Results should contain " + NPE,
        inferResults,
        containsExactly(
            NPE,
            SOURCE_FILE,
            methods
        )
    );
  }



}
