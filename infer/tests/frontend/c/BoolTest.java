/*
* Copyright (c) 2013 - present Facebook, Inc.
* All rights reserved.
*
* This source code is licensed under the BSD style license found in the
* LICENSE file in the root directory of this source tree. An additional grant
* of patent rights can be found in the PATENTS file in the same directory.
 */

package frontend.c;

import static org.hamcrest.MatcherAssert.assertThat;
import static utils.matchers.DotFilesEqual.dotFileEqualTo;

import com.google.common.collect.ImmutableList;

import org.junit.Rule;
import org.junit.Test;

import java.io.File;
import java.io.IOException;

import utils.DebuggableTemporaryFolder;
import utils.InferException;
import utils.InferRunner;

public class BoolTest {

  @Rule
  public DebuggableTemporaryFolder folder = new DebuggableTemporaryFolder();


  @Test
  public void whenCaptureRunCommaThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    String bool_src = "infer/tests/codetoanalyze/c/frontend/booleans/bool_example.c";

    String bool_dotty = "infer/tests/codetoanalyze/c/frontend/booleans/bool_example.dot";

    ImmutableList<String> inferCmd =
        InferRunner.createCInferCommandFrontend(
            folder,
            bool_src);
    File newDotFile = InferRunner.runInferFrontend(inferCmd);
    assertThat(
        "In the capture of " + bool_src +
            " the dotty files should be the same.",
        newDotFile, dotFileEqualTo(bool_dotty));
  }

  @Test
  public void whenCaptureBooleanConditionAsParamThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    String bool_src = "infer/tests/codetoanalyze/c/frontend/booleans/condition_as_param.c";

    String bool_dotty = "infer/tests/codetoanalyze/c/frontend/booleans/condition_as_param.dot";

    ImmutableList<String> inferCmd =
        InferRunner.createCInferCommandFrontend(
            folder,
            bool_src);
    File newDotFile = InferRunner.runInferFrontend(inferCmd);
    assertThat(
        "In the capture of " + bool_src +
            " the dotty files should be the same.",
        newDotFile, dotFileEqualTo(bool_dotty));
  }
}
