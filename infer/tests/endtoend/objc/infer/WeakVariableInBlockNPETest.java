/*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package endtoend.objc.infer;

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

public class WeakVariableInBlockNPETest {

  public static final String FILE =
      "infer/tests/codetoanalyze/objc/errors/npe/WeakCapturedVarsNPE.m";

  private static ImmutableList<String> inferCmdNPD;

  public static final String NULL_DEREFERENCE = "NULL_DEREFERENCE";

  @ClassRule
  public static DebuggableTemporaryFolder folderNPD =
  new DebuggableTemporaryFolder();

  @BeforeClass
  public static void runInfer() throws InterruptedException, IOException {
    inferCmdNPD = InferRunner.createiOSInferCommandWithMLBuckets(
        folderNPD,
        FILE,
        "cf",
        true);
  }

  @Test
  public void whenInferRunsOnWeakCapturedVariablesNPEThenNPEsFound()
      throws InterruptedException, IOException, InferException {
    InferResults inferResults = InferRunner.runInferC(inferCmdNPD);
    String[] procedures = {"__objc_anonymous_block_A_strongSelfNoCheck______2"};
    assertThat(
        "Results should not contain null dereference",
        inferResults,
        containsExactly(
            NULL_DEREFERENCE,
            FILE,
            procedures
        )
    );
  }
}
