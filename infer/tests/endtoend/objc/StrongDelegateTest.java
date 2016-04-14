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

public class StrongDelegateTest {

  public static final String FILE =
      "infer/tests/codetoanalyze/objc/warnings/strong_delegate.m";

  private static ImmutableList<String> inferCmdFraction;

  public static final String STRONG_DELEGATE_WARNING = "STRONG_DELEGATE_WARNING";

  @ClassRule
  public static DebuggableTemporaryFolder folder =
      new DebuggableTemporaryFolder();

  @BeforeClass
  public static void runInfer() throws InterruptedException, IOException {
    inferCmdFraction = InferRunner.createObjCInferCommand(
        folder,
        FILE);
  }

  @Test
  public void whenInferRunsOnStrongDelegate()
      throws InterruptedException, IOException, InferException {
    InferResults inferResults = InferRunner.runInferObjC(inferCmdFraction);
    assertThat(
        "Results should contain " + STRONG_DELEGATE_WARNING,
        inferResults,
        containsLines(new int[]{15, 19, 21, 23, 25}));
  }

}
