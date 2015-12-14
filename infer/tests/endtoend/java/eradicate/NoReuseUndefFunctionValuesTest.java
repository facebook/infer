/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package endtoend.java.eradicate;

import static org.hamcrest.MatcherAssert.assertThat;
import static utils.matchers.ResultContainsNoErrorInMethod.doesNotContain;

import org.junit.BeforeClass;
import org.junit.Test;

import java.io.IOException;

import utils.InferException;
import utils.InferResults;

public class NoReuseUndefFunctionValuesTest {

  public static final String SOURCE_FILE =
    "infer/tests/codetoanalyze/java/eradicate/NoReuseUndefFunctionValues.java";

  public static final String FIELD_NOT_INITIALIZED =
    "ERADICATE_FIELD_NOT_INITIALIZED";

  private static InferResults inferResults;

  @BeforeClass
  public static void loadResults() throws InterruptedException, IOException {
    inferResults =
      InferResults.loadEradicateResults(NoReuseUndefFunctionValuesTest.class, SOURCE_FILE);
  }

  @Test
  public void matchErrors() throws IOException, InterruptedException, InferException {
    assertThat(
      "Results should not contain " + FIELD_NOT_INITIALIZED,
      inferResults,
      doesNotContain(
        FIELD_NOT_INITIALIZED,
        SOURCE_FILE,
        "<init>"));
  }

}
