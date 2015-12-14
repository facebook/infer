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

import com.google.common.collect.ImmutableList;

import org.junit.BeforeClass;
import org.junit.ClassRule;
import org.junit.Test;

import java.io.IOException;

import utils.DebuggableTemporaryFolder;
import utils.InferException;
import utils.InferResults;
import utils.InferRunner;

public class ResourceLeakTest {

  public static final String memory_leak_file =
      "infer/tests/codetoanalyze/objc/errors/resource_leaks/ResourceLeakExample.m";

  private static ImmutableList<String> inferCmd;

  public static final String RESOURCE_LEAK = "RESOURCE_LEAK";

  @ClassRule
  public static DebuggableTemporaryFolder folder =
      new DebuggableTemporaryFolder();

  @BeforeClass
  public static void runInfer() throws InterruptedException, IOException {
    inferCmd = InferRunner.createiOSInferCommandWithMLBuckets(
        folder,
        memory_leak_file,
        "cf",
        false);
  }

  @Test
  public void whenInferRunsOnFileHandleForLoggingAtPathThenRLIsNotFound()
      throws InterruptedException, IOException, InferException {
    InferResults inferResults = InferRunner.runInferObjC(inferCmd);
    assertThat(
        "Results should not contain resource leak",
        inferResults,
        doesNotContain(
            RESOURCE_LEAK,
            memory_leak_file,
            "fileHandleForLoggingAtPath"));
  }

  @Test
  public void whenInferRunsOnNewOutputThenRLIsNotFound()
      throws InterruptedException, IOException, InferException {
    InferResults inferResults = InferRunner.runInferObjC(inferCmd);
    assertThat(
        "Results should not contain resource leak",
        inferResults,
        doesNotContain(
            RESOURCE_LEAK,
            memory_leak_file,
            "newOutput"));
  }

}
