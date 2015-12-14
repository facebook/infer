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

public class CADisplayLinkTest {

  public static final String CAD_FILE =
      "infer/tests/codetoanalyze/objc/errors/memory_leaks_benchmark/CADisplayLinkRetainCycle.m";

  private static ImmutableList<String> inferCmd;

  public static final String RETAIN_CYCLE = "RETAIN_CYCLE";

  @ClassRule
  public static DebuggableTemporaryFolder folder = new DebuggableTemporaryFolder();

  @BeforeClass
  public static void runInfer() throws InterruptedException, IOException {
    inferCmd = InferRunner.createObjCInferCommand(
        folder,
        CAD_FILE);
  }

  @Test
  public void RetainCycleShouldBeFound()
      throws InterruptedException, IOException, InferException {
    InferResults inferResults = InferRunner.runInferObjC(inferCmd);
    String[] methods = {
      "testCycle",
    };
    assertThat(
        "Results should contain " + RETAIN_CYCLE,
        inferResults,
        containsExactly(
            RETAIN_CYCLE,
            CAD_FILE,
            methods
        )
    );
  }

}
