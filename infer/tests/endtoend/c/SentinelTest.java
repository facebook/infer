/*
 * Copyright (c) 2013 - present Facebook, Inc.
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

public class SentinelTest {

  public static final String SOURCE_FILE =
      "attributes/sentinel.c";

  public static final String PREMATURE_NIL_TERMINATION_ARGUMENT =
      "PREMATURE_NIL_TERMINATION_ARGUMENT";

  private static InferResults inferResults;

  @BeforeClass
  public static void runInfer() throws InterruptedException, IOException {
    inferResults = InferResults.loadCInferResults(
        SentinelTest.class,
        SOURCE_FILE);
  }

  @Test
  public void whenInferRunsOnThenPNTAisFound()
      throws InterruptedException, IOException, InferException {
    String[] expectedPNTAProcedures = {"truncated_call"};

    assertThat(
        "Only PNTA should be found", inferResults,
        containsExactly(
            PREMATURE_NIL_TERMINATION_ARGUMENT,
            SOURCE_FILE,
            expectedPNTAProcedures));
  }
}
