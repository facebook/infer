/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package endtoend.cpp;

import static org.hamcrest.MatcherAssert.assertThat;
import static utils.matchers.ResultContainsExactly.containsExactly;

import com.google.common.collect.ImmutableList;

import org.junit.BeforeClass;
import org.junit.ClassRule;
import org.junit.Test;

import java.io.IOException;

import utils.DebuggableTemporaryFolder;
import utils.InferException;
import utils.InferResults;
import utils.InferRunner;

public class ConstructorNewTest {

  public static final String FILE =
      "infer/tests/codetoanalyze/cpp/frontend/constructors/constructor_new.cpp";

  private static ImmutableList<String> inferCmd;

  public static final String DIVIDE_BY_ZERO = "DIVIDE_BY_ZERO";

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
    String[] procedures = {
        "constructor_1_arg_new_div0",
        "constructor_3_args_new_div0",
        "int_init_number",
        "float_init_number",
        "int_init_empty",
        "int_init_empty_list",
        "int_init_empty_list_new",
        "int_init_nodes",
        "constructor_nodes",
        "int_array",
        "int_array_init"
    };
    assertThat(
        "Results should contain the expected divide by zero",
        inferResults,
        containsExactly(
            DIVIDE_BY_ZERO,
            FILE,
            procedures
        )
    );
  }
}
