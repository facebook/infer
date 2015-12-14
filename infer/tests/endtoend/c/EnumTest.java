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

import com.google.common.collect.ImmutableList;

import org.junit.BeforeClass;
import org.junit.ClassRule;
import org.junit.Test;

import java.io.IOException;

import utils.DebuggableTemporaryFolder;
import utils.InferException;
import utils.InferResults;
import utils.InferRunner;

public class EnumTest {

  public static final String SOURCE_FILE =
      "infer/tests/codetoanalyze/c/frontend/enumeration/other_enum.c";

  public static final String DIVIDE_BY_ZERO = "DIVIDE_BY_ZERO";
  private static ImmutableList<String> inferCmd;

  @ClassRule
  public static DebuggableTemporaryFolder folder =
      new DebuggableTemporaryFolder();

  @BeforeClass
  public static void runInfer() throws InterruptedException, IOException {
    inferCmd = InferRunner.createCInferCommand(folder, SOURCE_FILE);
  }

  @Test
  public void whenInferRunsOnDivideByZeroThenDivideByZeroIsFound()
      throws InterruptedException, IOException, InferException {
    InferResults inferResults = InferRunner.runInferC(inferCmd);
    String[] procedures = {"test"};
    assertThat(
        "Results should contain divide by zero error",
        inferResults,
        containsExactly(
            DIVIDE_BY_ZERO,
            SOURCE_FILE,
            procedures
        )
    );
  }


}
