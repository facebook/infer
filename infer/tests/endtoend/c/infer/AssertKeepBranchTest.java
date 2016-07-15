/*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package endtoend.c.infer;

import static org.hamcrest.MatcherAssert.assertThat;
import static utils.matchers.ResultContainsExactly.containsExactly;

import com.google.common.collect.ImmutableList;

import org.junit.BeforeClass;
import org.junit.Test;

import java.io.IOException;

import utils.InferException;
import utils.InferResults;

public class AssertKeepBranchTest {

  public static final String source_file = "assertions/assertion_example.c";

  private static ImmutableList<String> inferCmd;

  public static final String DIVIDE_BY_ZERO = "DIVIDE_BY_ZERO";

  private static InferResults inferResults;

  @BeforeClass
  public static void runInfer() throws InterruptedException, IOException {
    inferResults = InferResults.loadCInferResults(AssertKeepBranchTest.class, source_file);
  }

  @Test
  public void whenRunsOnAssertionExampleThenDiv0AndNoNPEIsFound()
      throws InterruptedException, IOException, InferException {
    String[] methods = {
        "report_div0_and_no_npe",
    };
    assertThat(
        "Results should contain " + DIVIDE_BY_ZERO,
        inferResults,
        containsExactly(
            DIVIDE_BY_ZERO,
            source_file,
            methods
        )
    );
  }

}
