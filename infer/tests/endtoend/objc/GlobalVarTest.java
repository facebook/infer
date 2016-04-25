/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package endtoend.objc;

import static org.hamcrest.MatcherAssert.assertThat;
import static utils.matchers.ResultContainsLineNumbers.containsLines;

import com.google.common.collect.ImmutableList;

import org.junit.BeforeClass;
import org.junit.ClassRule;
import org.junit.Test;

import java.io.IOException;

import utils.DebuggableTemporaryFolder;
import utils.InferException;
import utils.InferResults;
import utils.InferRunner;

public class GlobalVarTest {

  public static final String FILE =
      "infer/tests/codetoanalyze/objcpp/frontend/global-var/B.mm";

  private static ImmutableList<String> inferCmdFraction;

  public static final String GLOBAL_VARIABLE_INITIALIZED_WITH_FUNCTION_OR_METHOD_CALL = "GLOBAL_VARIABLE_INITIALIZED_WITH_FUNCTION_OR_METHOD_CALL";

  @ClassRule
  public static DebuggableTemporaryFolder folder =
      new DebuggableTemporaryFolder();

  @BeforeClass
  public static void runInfer() throws InterruptedException, IOException {
    inferCmdFraction = InferRunner.createObjCPPInferCommand(
        folder,
        FILE);
  }

  @Test
  public void whenInferRunsGlobalVar()
      throws InterruptedException, IOException, InferException {
    InferResults inferResults = InferRunner.runInferObjC(inferCmdFraction);
    assertThat(
        "Results should contain " + GLOBAL_VARIABLE_INITIALIZED_WITH_FUNCTION_OR_METHOD_CALL,
        inferResults,
        containsLines(new int[]{29, 31, 33}));
  }

}
