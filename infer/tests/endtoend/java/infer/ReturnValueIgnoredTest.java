/*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package endtoend.java.infer;


import static org.hamcrest.MatcherAssert.assertThat;
import static utils.matchers.ResultContainsErrorInMethod.contains;
import static utils.matchers.ResultContainsOnlyTheseErrors.containsOnly;

import org.junit.BeforeClass;
import org.junit.Test;

import java.io.IOException;

import utils.InferException;
import utils.InferResults;

public class ReturnValueIgnoredTest {

  public static final String ReturnValueIgnored =
      "infer/tests/codetoanalyze/java/infer/ReturnValueIgnored.java";

  public static final String RETURN_VALUE_IGNORED = "RETURN_VALUE_IGNORED";

  private static InferResults inferResults;

  @BeforeClass
  public static void loadResults() throws InterruptedException, IOException {
    inferResults = InferResults.loadInferResults(ReturnValueIgnoredTest.class, ReturnValueIgnored);
  }

  @Test
  public void returnValueIgnoredTest()
      throws IOException, InferException, InterruptedException {
    assertThat(
        "Results should contain a return value ignored error",
        inferResults,
        contains(
            RETURN_VALUE_IGNORED,
            ReturnValueIgnored,
            "returnValueIgnored"
        )
    );
  }

  @Test
  public void whenInferRunsOnReturnValueIgThenOnlyTheExpectedErrorsAreFound()
      throws InterruptedException, IOException, InferException {
    String[] expectedMethods = {"returnValueIgnored"};
    assertThat(
        "No unexpected errors should be found", inferResults,
        containsOnly(
            RETURN_VALUE_IGNORED,
            ReturnValueIgnored,
            expectedMethods));
  }

}
