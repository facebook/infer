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

public class BlockReleaseTest {

  public static final String FILE = "infer/tests/codetoanalyze/objc/frontend/block/block_release.m";

  private static ImmutableList<String> inferCmd;

  public static final String MEMORY_LEAK = "MEMORY_LEAK";

  @ClassRule
  public static DebuggableTemporaryFolder folder = new DebuggableTemporaryFolder();

  @BeforeClass
  public static void runInfer() throws InterruptedException, IOException {
    inferCmd = InferRunner.createObjCInferCommand(folder, FILE);
  }

  @Test
  public void whenInferRunsOnMnThenMemoryLeakIsNotFound()
      throws InterruptedException, IOException, InferException {
    InferResults inferResults = InferRunner.runInferObjC(inferCmd);
    String[] procedures = {
      // TODO: don't report on this test
      "blockReleaseTODO"
    };
    assertThat(
        "Results should contain the expected memory leak",
        inferResults,
        containsExactly(
            MEMORY_LEAK,
            FILE,
            procedures
        )
    );
  }
}
