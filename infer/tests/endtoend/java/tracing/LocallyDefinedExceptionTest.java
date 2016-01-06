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

public class LocallyDefinedExceptionTest {

  public static final String SOURCE_FILE =
      "infer/tests/codetoanalyze/java/tracing/LocallyDefinedExceptionExample.java";

  public static final String LOCALLY_DEFINED_EXCEPTION =
      "codetoanalyze.java.tracing.LocallyDefinedException";

  private static InferResults inferResults;

  @BeforeClass
  public static void loadResults() throws InterruptedException, IOException {
    inferResults = InferResults.loadTracingResults(
        LocallyDefinedExceptionTest.class,
        SOURCE_FILE);
  }

  @Test
  public void matchErrors()
      throws IOException, InterruptedException, InferException {
    String[] methods = {
        "fieldInvariant"
    };
    assertThat(
        "Results should contain " + LOCALLY_DEFINED_EXCEPTION,
        inferResults,
        containsExactly(
            LOCALLY_DEFINED_EXCEPTION,
            SOURCE_FILE,
            methods
        )
    );
  }


}
