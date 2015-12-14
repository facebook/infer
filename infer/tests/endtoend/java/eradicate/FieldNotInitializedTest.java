/*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package endtoend.java.eradicate;

import static org.hamcrest.MatcherAssert.assertThat;
import static utils.matchers.ResultContainsNumberOfErrorsInMethod.containsNumberOfErrors;

import org.junit.BeforeClass;
import org.junit.Test;

import java.io.IOException;

import utils.InferException;
import utils.InferResults;

public class FieldNotInitializedTest {

  public static final String SOURCE_FILE =
      "infer/tests/codetoanalyze/java/eradicate/FieldNotInitialized.java";

  public static final String FIELD_NOT_INITIALIZED =
      "ERADICATE_FIELD_NOT_INITIALIZED";

  private static InferResults inferResults;

  @BeforeClass
  public static void loadResults() throws InterruptedException, IOException {
    inferResults =
      InferResults.loadEradicateResults(FieldNotInitializedTest.class, SOURCE_FILE);
  }

  @Test
  public void matchErrors()
      throws IOException, InterruptedException, InferException {
    assertThat(
        "Results should contain " + FIELD_NOT_INITIALIZED,
        inferResults,
        containsNumberOfErrors(
            FIELD_NOT_INITIALIZED,
            SOURCE_FILE,
            "<init>",
            1));
  }

}
