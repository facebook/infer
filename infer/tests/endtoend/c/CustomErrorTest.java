/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package endtoend.c;

import static org.hamcrest.MatcherAssert.assertThat;
import static utils.matchers.ResultContainsExactly.containsExactly;

import org.junit.BeforeClass;
import org.junit.Test;

import java.io.IOException;

import utils.InferException;
import utils.InferResults;

public class CustomErrorTest {

  public static final String SOURCE_FILE =
      "custom_error/custom.c";

  public static final String CUSTOM_ERROR = "UNEXPECTED_NEGATIVE_EXPONENT";

  private static InferResults inferResults;

  @BeforeClass
  public static void runInfer() throws InterruptedException, IOException {
    inferResults = InferResults.loadCInferResults(CustomErrorTest.class, SOURCE_FILE);
  }

  @Test
  public void whenRunsOnAssertionFailureThenAssertionFailureIsFound()
      throws InterruptedException, IOException, InferException {
    String[] methods = {
        "paf",
        "pouf",
    };
    assertThat(
        "Results should contain " + CUSTOM_ERROR,
        inferResults,
        containsExactly(
            CUSTOM_ERROR,
            SOURCE_FILE,
            methods
        )
    );
  }

}
