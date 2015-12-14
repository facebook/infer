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
import static utils.matchers.ResultContainsErrorInMethod.contains;
import static utils.matchers.ResultContainsExactly.containsExactly;
import static utils.matchers.ResultContainsNoErrorInMethod.doesNotContain;

import com.google.common.collect.ImmutableList;

import org.junit.BeforeClass;
import org.junit.ClassRule;
import org.junit.Test;

import java.io.IOException;

import utils.DebuggableTemporaryFolder;
import utils.InferException;
import utils.InferResults;
import utils.InferRunner;

public class RetainCycleStaticVarTest {

  public static final String FILE =
      "infer/tests/codetoanalyze/objc/errors/memory_leaks_benchmark/RetainCycleStaticVar.m";

  private static ImmutableList<String> inferCmd;

  public static final String RETAIN_CYCLE = "RETAIN_CYCLE";

  @ClassRule
  public static DebuggableTemporaryFolder folder =
      new DebuggableTemporaryFolder();

  @BeforeClass
  public static void runInfer() throws InterruptedException, IOException {
    inferCmd = InferRunner.createObjCInferCommand(folder, FILE);
  }

  @Test
  public void whenInferRunsOnStrongCycleThenRCIsFound()
      throws InterruptedException, IOException, InferException {
    InferResults inferResults = InferRunner.runInferObjC(inferCmd);
    String[] procedures = {
        "main",
    };
    assertThat(
        "Results should contain the expected retain cycles",
        inferResults,
        containsExactly(
            RETAIN_CYCLE,
            FILE,
            procedures
        )
    );
  }
}
