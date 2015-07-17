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


public class NestedOperatorsTest {

  @Rule
  public DebuggableTemporaryFolder folder = new DebuggableTemporaryFolder();


  @Test
  public void whenCaptureRunEnumThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    String nestedassignment_expr =
        "infer/tests/codetoanalyze/c/frontend/" +
            "nestedoperators/nestedassignment.c";

    String nestedassignment_dotty =
        "infer/tests/codetoanalyze/c/frontend/" +
            "nestedoperators/nestedassignment.dot";

    ImmutableList<String> inferCmd =
        InferRunner.createCInferCommandFrontend(
            folder,
            nestedassignment_expr);
    File newDotFile = InferRunner.runInferFrontend(inferCmd);
    assertThat(
        "In the capture of " + nestedassignment_expr +
            " the dotty files should be the same.",
        newDotFile, dotFileEqualTo(nestedassignment_dotty));
  }

  @Test
  public void whenCaptureRunUnionThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    String nestedunion_expr =
        "infer/tests/codetoanalyze/c/frontend/nestedoperators/union.c";

    String nestedunion_dotty =
        "infer/tests/" +
            "codetoanalyze/c/frontend/nestedoperators/union.dot";

    ImmutableList<String> inferCmd =
        InferRunner.createCInferCommandFrontend(
            folder,
            nestedunion_expr);
    File newDotFile = InferRunner.runInferFrontend(inferCmd);
    assertThat(
        "In the capture of " + nestedunion_expr +
            " the dotty files should be the same.",
        newDotFile, dotFileEqualTo(nestedunion_dotty));
  }

  @Test
  public void whenCaptureRunAssignInConditionThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    String condition_assign_expr =
        "infer/tests/codetoanalyze/c/frontend/nestedoperators/assign_in_condition.c";

    String condition_assign_dotty =
        "infer/tests/" +
            "codetoanalyze/c/frontend/nestedoperators/assign_in_condition.dot";

    ImmutableList<String> inferCmd =
        InferRunner.createCInferCommandFrontend(
            folder,
            condition_assign_expr);
    File newDotFile = InferRunner.runInferFrontend(inferCmd);
    assertThat(
        "In the capture of " + condition_assign_expr +
            " the dotty files should be the same.",
        newDotFile, dotFileEqualTo(condition_assign_dotty));
  }

}
