/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package endtoend.java.infer;

import static org.hamcrest.MatcherAssert.assertThat;
import static utils.matchers.ResultContainsExactly.containsExactly;

import org.junit.BeforeClass;
import org.junit.Test;

import java.io.IOException;

import utils.InferException;
import utils.InferResults;

public class DynamicDispatchTest {

  public static final String DynamicDispatchFile =
      "infer/tests/codetoanalyze/java/infer/DynamicDispatch.java";

  public static final String NULL_DEREFERENCE = "NULL_DEREFERENCE";

  private static InferResults inferResults;

  @BeforeClass
  public static void loadResults() throws InterruptedException, IOException {
    inferResults = InferResults.loadInferResults(DynamicDispatchTest.class, DynamicDispatchFile);
  }

  @Test
  public void matchErrors()
      throws InterruptedException, IOException, InferException {
    String[] methods = {
      "interfaceShouldNotCauseFalseNegativeEasy",
      "dynamicDispatchShouldNotCauseFalseNegativeEasy",
      "interfaceShouldNotCauseFalseNegativeHard"
      // TODO: add dynamic dispatch support to make these tests work
      // "dynamicDispatchShouldNotCauseFalseNegativeHardTODO"
    };

    assertThat(
        "Results should contain null dereference",
        inferResults,
        containsExactly(
            NULL_DEREFERENCE,
            DynamicDispatchFile,
            methods
        )
    );
  }

}
