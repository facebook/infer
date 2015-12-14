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

import com.google.common.collect.ImmutableList;

import org.junit.BeforeClass;
import org.junit.ClassRule;
import org.junit.Test;

import java.io.IOException;

import utils.DebuggableTemporaryFolder;
import utils.InferException;
import utils.InferResults;
import utils.InferRunner;

public class ProcdescTest {

  public static final String MAIN_FILE =
      "infer/tests/" +
          "codetoanalyze/objc/errors/procdescs/main.c";

  private static ImmutableList<String> inferCmd;

  public static final String MEMORY_LEAK = "MEMORY_LEAK";

  @ClassRule
  public static DebuggableTemporaryFolder folder = new DebuggableTemporaryFolder();

  @BeforeClass
  public static void runInfer() throws InterruptedException, IOException {
    inferCmd = InferRunner.createObjCInferCommand(folder, MAIN_FILE);
  }

  @Test
  public void whenInferRunsOnNull_deref_objc_classThenMemoryLeakIsFound()
      throws InterruptedException, IOException, InferException {
    InferResults inferResults = InferRunner.runInferC(inferCmd);
    assertThat(
        "Results should contain a memory leak. " +
            "This shows that it doesn't stop because of procdesc not found.",
        inferResults,
        contains(
            MEMORY_LEAK,
            MAIN_FILE,
            "main"
        )
    );
  }

  @Test
  public void whenInferRunsOnCall_nslogThenMemoryLeakIsFound()
      throws InterruptedException, IOException, InferException {
    InferResults inferResults = InferRunner.runInferC(inferCmd);
    assertThat(
        "Results should contain a memory leak. " +
            "This shows that it doesn't stop because of procdesc not found.",
        inferResults,
        contains(
            MEMORY_LEAK,
            MAIN_FILE,
            "call_nslog"
        )
    );
  }


}
