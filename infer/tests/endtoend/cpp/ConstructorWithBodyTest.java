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

public class ConstructorWithBodyTest {

  public static final String FILE =
      "infer/tests/codetoanalyze/cpp/frontend/constructors/constructor_with_body.cpp";

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
  public void whenInferRunsOnConstructorWithBodyDiv0ErrorIsFound()
      throws InterruptedException, IOException, InferException {
    InferResults inferResults = InferRunner.runInferCPP(inferCmd);
    String[] procedures = {
        "test_div0",
        "test_div0_default_constructor",
    };
    assertThat(
        "Results should contain null pointer dereference error",
        inferResults,
        contains(
            DIVIDE_BY_ZERO,
            FILE,
            "test_div0"
        )
    );
    assertThat(
        "Results should contain null pointer dereference error",
        inferResults,
        contains(
            DIVIDE_BY_ZERO,
            FILE,
            "test_div0_default_constructor"
        )
    );
  }

  @Test
  public void whenInferRunsOnConstructorWithBodyDiv1ErrorIsNotFound()
      throws InterruptedException, IOException, InferException {
    InferResults inferResults = InferRunner.runInferCPP(inferCmd);
    assertThat(
        "Results should not contain null pointer dereference error",
        inferResults,
        doesNotContain(
            DIVIDE_BY_ZERO,
            FILE,
            "test_div1"
        )
    );
  }
}
