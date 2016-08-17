/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package endtoend.objcpp.linters;

import static org.hamcrest.MatcherAssert.assertThat;
import static utils.matchers.ResultContainsLineNumbers.containsOnlyLines;

import com.google.common.collect.ImmutableList;

import org.junit.BeforeClass;
import org.junit.ClassRule;
import org.junit.Test;

import java.io.IOException;

import utils.DebuggableTemporaryFolder;
import utils.InferException;
import utils.InferResults;
import utils.InferRunner;

public class BlockCaptureCXXRefTest {

  public static final String FILE =
      "infer/tests/codetoanalyze/objcpp/linters/cxx_reference_in_block/block.mm";

  private static ImmutableList<String> inferCmd;

  public static final String REFERENCE_CAPTURED = "CXX_REFERENCE_CAPTURED_IN_OBJC_BLOCK";

  @ClassRule
  public static DebuggableTemporaryFolder folder =
      new DebuggableTemporaryFolder();

  @BeforeClass
  public static void runInfer() throws InterruptedException, IOException {
    inferCmd = InferRunner.createObjCPPLintersCommand(folder, FILE);
  }

  @Test
  public void matchErrors()
      throws InterruptedException, IOException, InferException {
    InferResults inferResults = InferRunner.runInferObjC(inferCmd);
    assertThat(
        "Results should contain the correct " + REFERENCE_CAPTURED,
        inferResults,
        containsOnlyLines(
            new int[]{
                20,
                37
            }));
  }
}
