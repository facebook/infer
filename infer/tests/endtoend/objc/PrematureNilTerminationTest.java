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
import static utils.matchers.ResultContainsNoErrorInMethod.doesNotContain;
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

public class PrematureNilTerminationTest {

  public static final String SOURCE_FILE =
      "infer/tests/codetoanalyze/objc/errors/variadic_methods/premature_nil_termination.m";

  private static ImmutableList<String> inferCmd;

  public static final String PREMATURE_NIL_TERMINATION_ARGUMENT =
      "PREMATURE_NIL_TERMINATION_ARGUMENT";

  @ClassRule
  public static DebuggableTemporaryFolder folderNPD = new DebuggableTemporaryFolder();

  @BeforeClass
  public static void runInfer() throws InterruptedException, IOException {
    inferCmd = InferRunner.createObjCInferCommandWithMLBuckets(
        folderNPD,
        SOURCE_FILE,
        "cf",
        true);
  }

  @Test
  public void whenInferRunsOnPrematureNileFileThenOnePNTAIsFound()
      throws InterruptedException, IOException, InferException {
    InferResults inferResults = InferRunner.runInferObjC(inferCmd);
    String[] expectedPNTAProcedures = {"nilInArrayWithObjects"};

    assertThat(
        "Only PNTA should be found", inferResults,
        containsExactly(
            PREMATURE_NIL_TERMINATION_ARGUMENT,
            SOURCE_FILE,
            expectedPNTAProcedures));
  }

}
