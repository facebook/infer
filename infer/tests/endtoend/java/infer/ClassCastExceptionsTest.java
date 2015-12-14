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
import static utils.matchers.ResultContainsNoErrorInMethod.doesNotContain;
import static utils.matchers.ResultContainsOnlyTheseErrors.containsOnly;

import org.junit.BeforeClass;
import org.junit.Test;

import java.io.IOException;

import utils.InferException;
import utils.InferResults;

public class ClassCastExceptionsTest {

  public static final String ClassCastExceptions =
      "infer/tests/codetoanalyze/java/infer/ClassCastExceptions.java";

  public static final String CLASS_CAST_EXCEPTION = "CLASS_CAST_EXCEPTION";

  private static InferResults inferResults;

  @BeforeClass
  public static void loadResults() throws InterruptedException, IOException {
    inferResults = InferResults.loadInferResults(
        ClassCastExceptionsTest.class,
        ClassCastExceptions);
  }

  @Test
  public void whenInferRunsOnClassCastExceptionThenCCEFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should contain class cast exception.",
        inferResults,
        contains(
            CLASS_CAST_EXCEPTION,
            ClassCastExceptions,
            "classCastException"
        )
    );
  }

  @Test
  public void whenInferRunsOnClassCastExceptionImplementsIThenCCEErrorFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should contain class cast exception.",
        inferResults,
        contains(
            CLASS_CAST_EXCEPTION,
            ClassCastExceptions,
            "classCastExceptionImplementsInterface"
        )
    );
  }

  @Test
  public void whenInferRunsOnOpenHttpURLConnectionThenCCEErrorNotFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should not contain class cast exception.",
        inferResults,
        doesNotContain(
            CLASS_CAST_EXCEPTION,
            ClassCastExceptions,
            "openHttpURLConnection"
        )
    );
  }

  @Test
  public void whenInferIsRunCCEThenOnlyTheExpectedErrorsAreFound()
      throws InterruptedException, IOException, InferException {
    String[] expectedMethods = {
        "classCastException",
        "classCastExceptionImplementsInterface",
        "openHttpURLConnection"
    };
    assertThat(
        "No unexpected errors should be found",
        inferResults,
        containsOnly(
            CLASS_CAST_EXCEPTION,
            ClassCastExceptions,
            expectedMethods));
  }

}
