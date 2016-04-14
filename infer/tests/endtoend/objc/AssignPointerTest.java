/*
 * Copyright (c) 2016 - present Facebook, Inc.
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

public class AssignPointerTest {

  public static final String FILE =
      "infer/tests/codetoanalyze/objc/warnings/assign_pointer.m";

  private static ImmutableList<String> inferCmd;

  public static final String ASSIGN_POINTER_WARNING = "ASSIGN_POINTER_WARNING";

  @ClassRule
  public static DebuggableTemporaryFolder folder =
      new DebuggableTemporaryFolder();

  @BeforeClass
  public static void runInfer() throws InterruptedException, IOException {
    inferCmd = InferRunner.createiOSInferCommandWithMLBuckets(
      folder,
      FILE,
      "cf",
      true);

  }

  @Test
  public void matchErrors()
      throws InterruptedException, IOException, InferException {
    InferResults inferResults = InferRunner.runInferObjC(inferCmd);
    assertThat(
        "Results should contain the correct " + ASSIGN_POINTER_WARNING,
        inferResults,
        containsLines(new int[]{
            18,
            20,
            22
          }));
  }

}
