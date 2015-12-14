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
import static utils.matchers.ResultContainsExactly.containsExactly;

import org.junit.BeforeClass;
import org.junit.Test;

import java.io.IOException;

import utils.InferException;
import utils.InferResults;

public class ParameterNotNullableTest {

  public static final String SOURCE_FILE =
      "infer/tests/codetoanalyze/java/eradicate/ParameterNotNullable.java";

  public static final String PARAMETER_NOT_NULLABLE =
      "ERADICATE_PARAMETER_NOT_NULLABLE";

  private static InferResults inferResults;

  @BeforeClass
  public static void loadResults() throws InterruptedException, IOException {
    inferResults =
      InferResults.loadEradicateResults(ParameterNotNullableTest.class, SOURCE_FILE);
  }

  @Test
  public void matchErrors()
      throws IOException, InterruptedException, InferException {
    String[] methods = {
        "callNull",
        "callNullable",
        "testSystemGetPropertyArgument",
        "testClassGetResourceArgument",
    };
    assertThat(
        "Results should contain " + PARAMETER_NOT_NULLABLE,
        inferResults,
        containsExactly(
            PARAMETER_NOT_NULLABLE,
            SOURCE_FILE,
            methods));
  }

}
