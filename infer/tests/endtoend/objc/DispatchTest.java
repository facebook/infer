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

public class DispatchTest {

  public static final String NPE_FILE =
      "infer/tests/codetoanalyze/objc/frontend/block/dispatch_examples.m";

  private static ImmutableList<String> inferCmdNPD;

  public static final String NULL_DEREFERENCE = "NULL_DEREFERENCE";

  @ClassRule
  public static DebuggableTemporaryFolder folderNPD = new DebuggableTemporaryFolder();

  @BeforeClass
  public static void runInfer() throws InterruptedException, IOException {
    inferCmdNPD = InferRunner.createiOSInferCommandWithMLBuckets(
        folderNPD,
        NPE_FILE,
        "cf",
        true);
  }

  @Test
  public void whenInferRunsOnDispatch_once_exampleThenNoNPENotFound()
      throws InterruptedException, IOException, InferException {
    InferResults inferResults = InferRunner.runInferObjC(inferCmdNPD);
    assertThat(
        "NPE should not be found", inferResults,
        doesNotContain(
            NULL_DEREFERENCE,
            NPE_FILE,
            "dispatch_once_example"));
  }

  @Test
  public void whenInferRunsOnDispatch_async_exampleThenNoNPENotFound()
      throws InterruptedException, IOException, InferException {
    InferResults inferResults = InferRunner.runInferObjC(inferCmdNPD);
    assertThat(
        "NPE should not be found", inferResults,
        doesNotContain(
            NULL_DEREFERENCE,
            NPE_FILE,
            "dispatch_async_example"));
  }

  @Test
  public void whenInferRunsOnDispatch_after_exampleThenNoNPENotFound()
      throws InterruptedException, IOException, InferException {
    InferResults inferResults = InferRunner.runInferObjC(inferCmdNPD);
    assertThat(
        "NPE should not be found", inferResults,
        doesNotContain(
            NULL_DEREFERENCE,
            NPE_FILE,
            "dispatch_after_example"));
  }

  @Test
  public void whenInferRunsOnDispatch_group_exampleThenNoNPENotFound()
      throws InterruptedException, IOException, InferException {
    InferResults inferResults = InferRunner.runInferObjC(inferCmdNPD);
    assertThat(
        "NPE should not be found", inferResults,
        doesNotContain(
            NULL_DEREFERENCE,
            NPE_FILE,
            "dispatch_group_example"));
  }

  @Test
  public void whenInferRunsOnDispatch_group_notify_exampleThenNoNPENotFound()
      throws InterruptedException, IOException, InferException {
    InferResults inferResults = InferRunner.runInferObjC(inferCmdNPD);
    assertThat(
        "NPE should not be found", inferResults,
        doesNotContain(
            NULL_DEREFERENCE,
            NPE_FILE,
            "dispatch_group_notify_example"));
  }

  @Test
  public void whenInferRunsOnDispatch_barrier_exampleThenNoNPENotFound()
      throws InterruptedException, IOException, InferException {
    InferResults inferResults = InferRunner.runInferObjC(inferCmdNPD);
    assertThat(
        "NPE should not be found", inferResults,
        doesNotContain(
            NULL_DEREFERENCE,
            NPE_FILE,
            "dispatch_barrier_example"));
  }



}
