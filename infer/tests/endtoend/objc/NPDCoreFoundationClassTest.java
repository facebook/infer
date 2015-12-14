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
import static utils.matchers.ResultContainsOnlyTheseErrors.containsOnly;

import com.google.common.collect.ImmutableList;

import org.junit.BeforeClass;
import org.junit.ClassRule;
import org.junit.Test;

import java.io.IOException;

import utils.DebuggableTemporaryFolder;
import utils.InferException;
import utils.InferResults;
import utils.InferRunner;

public class NPDCoreFoundationClassTest {

  public static final String NPD_FILE =
      "infer/tests/codetoanalyze/objc/errors/npe/NPD_core_foundation.m";

  private static ImmutableList<String> inferCmdNPD;

  public static final String NULL_DEREFERENCE = "NULL_DEREFERENCE";

  @ClassRule
  public static DebuggableTemporaryFolder folderNPD =
      new DebuggableTemporaryFolder();

  @BeforeClass
  public static void runInfer() throws InterruptedException, IOException {
    inferCmdNPD = InferRunner.createiOSInferCommandWithMLBuckets(
        folderNPD,
        NPD_FILE,
        "cf",
        false);

  }


  @Test
  public void whenInferRunsTest2ThenOnlyTheExpectedErrorsAreFound()
      throws InterruptedException, IOException, InferException {
    InferResults inferResults = InferRunner.runInferC(inferCmdNPD);
    String[] expectedProcedures = {
        "NullDeref_test2"
    };
    assertThat(
        "No unexpected errors should be found", inferResults,
        containsOnly(
            NULL_DEREFERENCE,
            NPD_FILE,
            expectedProcedures));
  }

}
