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

public class VectorEmptyAccessTest {

  public static final String FILE =
      "infer/tests/codetoanalyze/cpp/errors/vector/empty_access.cpp";

  private static ImmutableList<String> inferCmd;

  public static final String EMPTY_VECTOR_ACCESS = "EMPTY_VECTOR_ACCESS";

  @ClassRule
  public static DebuggableTemporaryFolder folder =
      new DebuggableTemporaryFolder();

  @BeforeClass
  public static void runInfer() throws InterruptedException, IOException {
    inferCmd = InferRunner.createCPPInferCommand(folder, FILE);
  }

  @Test
  public void whenInferRunsNullDerefFunctionsErrorIsFound()
      throws InterruptedException, IOException, InferException {
    String[] procedures = {
        "access_empty",
        "clear_empty",
        //"resize0_empty", // resize(0) doesn't make vector empty
        "copy_empty",
        "assign_empty",
        "empty_check_access_empty",
        "size_check0_empty",
        "vector_as_param_empty",
        "vector_as_param_clear",
        "vector_as_param_by_value_empty",
        "getter_empty",
    };
    InferResults inferResults = InferRunner.runInferCPP(inferCmd);
    assertThat(
        "Results should contain empty vector access",
        inferResults,
        containsExactly(
            EMPTY_VECTOR_ACCESS,
            FILE,
            procedures
        )
    );
  }
}
