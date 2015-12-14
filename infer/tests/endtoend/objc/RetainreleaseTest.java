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

public class RetainreleaseTest {

  public static final String retain_release_file =
      "infer/tests/" +
          "codetoanalyze/objc/errors/memory_leaks_benchmark/RetainReleaseExample2.m";

  private static ImmutableList<String> inferCmd;

  public static final String MEMORY_LEAK = "MEMORY_LEAK";

  @ClassRule
  public static DebuggableTemporaryFolder folder =
      new DebuggableTemporaryFolder();

  @BeforeClass
  public static void runInfer() throws InterruptedException, IOException {
    inferCmd = InferRunner.createObjCInferCommand(
        folder,
        retain_release_file);
  }


  @Test
  public void whenInferRunsOnSimpleTest3ThenMLIsFound()
      throws InterruptedException, IOException, InferException {
    InferResults inferResults = InferRunner.runInferObjC(inferCmd);
    assertThat(
        "Results should contain memory leak error",
        inferResults,
        contains(
            MEMORY_LEAK,
            retain_release_file,
            "test3"
        )
    );
  }

  @Test
  public void whenInferRunsOnSimpleTest6ThenMLIsFound()
      throws InterruptedException, IOException, InferException {
    InferResults inferResults = InferRunner.runInferObjC(inferCmd);
    assertThat(
        "Results should contain memory leak error",
        inferResults,
        contains(
            MEMORY_LEAK,
            retain_release_file,
            "test6"
        )
    );
  }

}
