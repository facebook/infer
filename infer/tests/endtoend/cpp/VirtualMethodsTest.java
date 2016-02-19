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

public class VirtualMethodsTest {

  public static final String FILE =
      "infer/tests/codetoanalyze/cpp/frontend/methods/virtual_methods.cpp";

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
  public void whenInferRunsOnMethodsDivideByZeroIsFound()
      throws InterruptedException, IOException, InferException {
    InferResults inferResults = InferRunner.runInferCPP(inferCmd);
    String[] proceduresWithNullDeref = {
      "rect_area", "tri_area", "poly_area", "tri_not_virtual_area"
    };
    assertThat(
        "Results should contain the no null dereference",
        inferResults,
        containsExactly(
            DIVIDE_BY_ZERO,
            FILE,
            proceduresWithNullDeref
        )
    );
  }
}
