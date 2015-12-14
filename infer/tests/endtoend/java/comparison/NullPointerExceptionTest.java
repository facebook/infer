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
import static utils.matchers.ResultContainsTheseErrors.contains;

import org.junit.BeforeClass;
import org.junit.Test;

import java.io.IOException;

import utils.InferException;
import utils.InferResults;

public class NullPointerExceptionTest {

  public static final String SOURCE_FILE =
      "infer/tests/codetoanalyze/java/infer/NullPointerExceptions.java";

  public static final String NPE =
      "java.lang.NullPointerException";

  private static InferResults inferResults;

  @BeforeClass
  public static void loadResults() throws InterruptedException, IOException {
    inferResults = InferResults.loadTracingComparisonResults(
        NullPointerExceptionTest.class,
        SOURCE_FILE);
  }

  @Test
  public void errorsFoundByInferExpectedToBeFoundInTracingMode()
      throws IOException, InterruptedException, InferException {
    String[] methods = {
        "nullPointerException",
        "nullPointerExceptionInterProc",
        "nullPointerExceptionWithExceptionHandling",
        "nullPointerExceptionWithArray",
        "nullPointerExceptionWithNullObjectParameter",
        "nullPointerExceptionWithNullArrayParameter",
        "nullPointerExceptionFromFaillingResourceConstructor",
        "nullPointerExceptionFromFailingFileOutputStreamConstructor",
        "nullPointerExceptionUnlessFrameFails",
    };
    assertThat(
        "Results should contain " + NPE,
        inferResults,
        contains(
            NPE,
            SOURCE_FILE,
            methods
        )
    );
  }


}
