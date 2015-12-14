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

public class ParameterNotNullableTest {

  public static final String FILE =
      "infer/tests/codetoanalyze/objc/warnings/ParameterNotNullableExample.m";

  private static ImmutableList<String> inferCmdFraction;

  public static final String PARAMETER_NOT_NULL_CHECKED = "PARAMETER_NOT_NULL_CHECKED";

  public static final String IVAR_NOT_NULL_CHECKED = "IVAR_NOT_NULL_CHECKED";

  @ClassRule
  public static DebuggableTemporaryFolder folderFraction =
      new DebuggableTemporaryFolder();

  @BeforeClass
  public static void runInfer() throws InterruptedException, IOException {
    inferCmdFraction = InferRunner.createObjCInferCommand(
        folderFraction,
        FILE);
  }

  @Test
  public void whenInferRunsOnFBAudioInputCallbackSimpleThenPNNIsFound()
      throws InterruptedException, IOException, InferException {
    InferResults inferResults = InferRunner.runInferObjC(inferCmdFraction);
    assertThat(
        "Results should contain a parameter not nullable warning",
        inferResults,
        contains(
            PARAMETER_NOT_NULL_CHECKED,
            FILE,
            "FBAudioInputCallbackSimple:"
        )
    );
  }

  @Test
  public void whenInferRunsOnFBAudioInputCallbackSimpleAliasingThenPNNIsFound()
      throws InterruptedException, IOException, InferException {
    InferResults inferResults = InferRunner.runInferObjC(inferCmdFraction);
    assertThat(
        "Results should contain a parameter not nullable warning",
        inferResults,
        contains(
            PARAMETER_NOT_NULL_CHECKED,
            FILE,
            "FBAudioInputCallbackSimpleAliasing:"
        )
    );
  }

  @Test
     public void whenInferRunsOnFBAudioInputCallbackChainThenPNNIsFound()
      throws InterruptedException, IOException, InferException {
    InferResults inferResults = InferRunner.runInferObjC(inferCmdFraction);
    assertThat(
        "Results should contain a parameter not nullable warning",
        inferResults,
        contains(
            PARAMETER_NOT_NULL_CHECKED,
            FILE,
            "FBAudioInputCallbackChain:"
        )
    );
  }

  @Test
  public void whenInferRunsOnInitThenPNNIsNotFound()
      throws InterruptedException, IOException, InferException {
    InferResults inferResults = InferRunner.runInferObjC(inferCmdFraction);
    assertThat(
        "Results should not contain a parameter not nullable warning",
        inferResults,
        doesNotContain(
            PARAMETER_NOT_NULL_CHECKED,
            FILE,
            "init"
        )
    );
  }

  @Test
  public void whenInferRunsOnTestThenPNNIsNotFound()
      throws InterruptedException, IOException, InferException {
    InferResults inferResults = InferRunner.runInferObjC(inferCmdFraction);
    assertThat(
        "Results should not contain a parameter not nullable warning",
        inferResults,
        doesNotContain(
            PARAMETER_NOT_NULL_CHECKED,
            FILE,
            "test"
        )
    );
  }

  @Test
  public void whenInferRunsOnFBAudioInputCallbackFieldThenIVNNIsNotFound()
      throws InterruptedException, IOException, InferException {
    InferResults inferResults = InferRunner.runInferObjC(inferCmdFraction);
    assertThat(
        "Results should contain an ivar not nullable warning",
        inferResults,
        contains(
            IVAR_NOT_NULL_CHECKED,
            FILE,
            "FBAudioInputCallbackField"
        )
    );
  }

  @Test
  public void whenInferRunsOnFBAudioInputCallbackChainThenIVNNIsNotFound()
      throws InterruptedException, IOException, InferException {
    InferResults inferResults = InferRunner.runInferObjC(inferCmdFraction);
    assertThat(
        "Results should contain an ivar not nullable warning",
        inferResults,
        contains(
            IVAR_NOT_NULL_CHECKED,
            FILE,
            "FBAudioInputCallbackChain:"
        )
    );
  }

}
