/*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package endtoend.objc;

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

public class NPEObjCMethodInConditionTest {

  public static final String NPE_FILE =
      "infer/tests/codetoanalyze/objc/errors/npe/ObjCMethodCallInCondition.m";

  private static ImmutableList<String> inferCmdNPD;

  public static final String PARAMETER_NOT_NULL_CHECKED = "PARAMETER_NOT_NULL_CHECKED";

  @ClassRule
  public static DebuggableTemporaryFolder folderNPD = new DebuggableTemporaryFolder();

  @BeforeClass
  public static void runInfer() throws InterruptedException, IOException {
    inferCmdNPD = InferRunner.createiOSInferCommandWithMLBuckets(
        folderNPD,
        NPE_FILE,
        "cf",
        true);
  }

  @Test
  public void whenInferRunsOncaptureManagerSessionDidStartThenNoNPEFound()
      throws InterruptedException, IOException, InferException {
    InferResults inferResults = InferRunner.runInferObjC(inferCmdNPD);
    String[] procedures = {};
    assertThat(
        "Results should not contain parameter not null checked",
        inferResults,
        containsExactly(
            PARAMETER_NOT_NULL_CHECKED,
            NPE_FILE,
            procedures
        )
    );
  }
}
