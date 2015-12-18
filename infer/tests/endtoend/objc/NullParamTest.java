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

public class NullParamTest {

  public static final String block_file =
     "infer/tests/codetoanalyze/objc/errors/npe/blockenum.m";

  private static ImmutableList<String> inferCmd;

  public static final String PARAMETER_NOT_NULL_CHECKED = "PARAMETER_NOT_NULL_CHECKED";

  @ClassRule
  public static DebuggableTemporaryFolder folder =
      new DebuggableTemporaryFolder();

  @BeforeClass
  public static void runInfer() throws InterruptedException, IOException {
    inferCmd = InferRunner.createObjCInferCommand(
        folder,
        block_file
    );
  }

  @Test
  public void whenInferRunsOnFoo1ParameterNilIsNotFound()
      throws InterruptedException, IOException, InferException {
    InferResults inferResults = InferRunner.runInferObjC(inferCmd);
    assertThat(
        "Results should not contain memory leak",
        inferResults,
        doesNotContain(
            PARAMETER_NOT_NULL_CHECKED,
            block_file,
            "foo1:"));
  }
  @Test
  public void whenInferRunsOnAllResultsListParameterNilIsNotFound()
      throws InterruptedException, IOException, InferException {
    InferResults inferResults = InferRunner.runInferObjC(inferCmd);
    assertThat(
        "Results should not contain memory leak",
        inferResults,
        doesNotContain(
            PARAMETER_NOT_NULL_CHECKED,
            block_file,
            "allResultsList:"));
  }
}
