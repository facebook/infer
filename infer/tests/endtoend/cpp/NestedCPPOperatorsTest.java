/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package endtoend.cpp;

import static org.hamcrest.MatcherAssert.assertThat;
import static utils.matchers.ResultContainsErrorInMethod.contains;
import static utils.matchers.ResultContainsNoErrorInMethod.doesNotContain;

import com.google.common.collect.ImmutableList;

import org.junit.BeforeClass;
import org.junit.ClassRule;
import org.junit.Test;

import java.io.IOException;

import utils.DebuggableTemporaryFolder;
import utils.InferException;
import utils.InferResults;
import utils.InferRunner;

public class NestedCPPOperatorsTest {

  public static final String FILE =
      "infer/tests/codetoanalyze/cpp/frontend/nestedoperators/var_decl_inside_if.cpp";

  private static ImmutableList<String> inferCmd;

  public static final String DIVIDE_BY_ZERO = "DIVIDE_BY_ZERO";
  public static final String NULL_DEREFERENCE = "NULL_DEREFERENCE";

  @ClassRule
  public static DebuggableTemporaryFolder folder =
      new DebuggableTemporaryFolder();

  @BeforeClass
  public static void runInfer() throws InterruptedException, IOException {
    inferCmd = InferRunner.createCPPInferCommand(folder, FILE);
  }

  @Test
  public void whenInferRunsOnDiv0MethodsErrorIsFound()
      throws InterruptedException, IOException, InferException {
    InferResults inferResults = InferRunner.runInferCPP(inferCmd);
    String[] proceduresWithDiv0 = {
        "simple_init_div0",
        "simple_inif_elseif_div0",
        "function_call_init_div0",
        "conditional_init_div0",
        "reference_init_div0",
    };
    for (String procedure : proceduresWithDiv0) {
      assertThat(
        "Results should contain the expected divide by zero",
        inferResults,
        contains(
          DIVIDE_BY_ZERO,
          FILE,
          procedure
        )
      );
    }
    /* null dereference */
    assertThat(
      "Results should contain the expected null dereference",
      inferResults,
      contains(
        NULL_DEREFERENCE,
        FILE,
        "simple_init_null_deref"
      )
    );
  }
}
