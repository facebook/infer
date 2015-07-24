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

import org.junit.Ignore;
import org.junit.Rule;
import org.junit.Test;

import java.io.File;
import java.io.IOException;

import utils.DebuggableTemporaryFolder;
import utils.InferException;
import utils.InferRunner;

public class ConditionalOperatorTest {

  @Rule
  public DebuggableTemporaryFolder folder = new DebuggableTemporaryFolder();


  @Test
  public void whenCaptureRunCommaThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    String cond_src =
        "infer/tests/codetoanalyze/c/frontend/conditional_operator/conditional_operator.c";

    String cond_dotty =
        "infer/tests/codetoanalyze/c/frontend/conditional_operator/conditional_operator.dot";

    ImmutableList<String> inferCmd =
        InferRunner.createCInferCommandFrontend(
            folder,
            cond_src);
    File newDotFile = InferRunner.runInferFrontend(inferCmd);
    assertThat(
        "In the capture of " + cond_src + " the dotty files should be the same.",
        newDotFile, dotFileEqualTo(cond_dotty));
  }

  @Test
  public void whenCaptureRunShortCThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    String cond_src =
        "infer/tests/codetoanalyze/c/frontend/conditional_operator/if_short_circuit.c";

    String cond_dotty =
        "infer/tests/codetoanalyze/c/frontend/conditional_operator/if_short_circuit.dot";

    ImmutableList<String> inferCmd =
        InferRunner.createCInferCommandFrontend(
            folder,
            cond_src);
    File newDotFile = InferRunner.runInferFrontend(inferCmd);
    assertThat(
        "In the capture of " + cond_src + " the dotty files should be the same.",
        newDotFile, dotFileEqualTo(cond_dotty));
  }

  @Test
  public void whenCaptureRunCond2ThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    String cond_src =
        "infer/tests/codetoanalyze/c/frontend/conditional_operator/cond2.c";

    String cond_dotty =
        "infer/tests/codetoanalyze/c/frontend/conditional_operator/cond2.dot";

    ImmutableList<String> inferCmd =
        InferRunner.createCInferCommandFrontend(
            folder,
            cond_src);
    File newDotFile = InferRunner.runInferFrontend(inferCmd);
    assertThat(
        "In the capture of " + cond_src + " the dotty files should be the same.",
        newDotFile, dotFileEqualTo(cond_dotty));
  }

  @Ignore @Test
  public void whenCaptureRunOnAssertExampleThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    String cond_src =
        "infer/tests/codetoanalyze/c/frontend/conditional_operator/assert_example.c";

    String cond_dotty =
        "infer/tests/codetoanalyze/c/frontend/conditional_operator/assert_example.dot";

    ImmutableList<String> inferCmd =
        InferRunner.createCInferCommandFrontend(
            folder,
            cond_src);
    File newDotFile = InferRunner.runInferFrontend(inferCmd);
    assertThat(
        "In the capture of " + cond_src + " the dotty files should be the same.",
        newDotFile, dotFileEqualTo(cond_dotty));
  }

    @Test
    public void whenCaptureRunOnIntNegationThenDotFilesAreTheSame()
    throws InterruptedException, IOException, InferException {
        String cond_src =
        "infer/tests/codetoanalyze/c/frontend/conditional_operator/int_negation.c";

        String cond_dotty =
        "infer/tests/codetoanalyze/c/frontend/conditional_operator/int_negation.dot";

        ImmutableList<String> inferCmd =
        InferRunner.createCInferCommandFrontend(
                                                folder,
                                                cond_src);
        File newDotFile = InferRunner.runInferFrontend(inferCmd);
        assertThat(
                   "In the capture of " + cond_src + " the dotty files should be the same.",
                   newDotFile, dotFileEqualTo(cond_dotty));
    }

  @Test
  public void whenCaptureRunBinaryOperatorThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    String cond_src =
        "infer/tests/codetoanalyze/c/frontend/conditional_operator/binary_operator.c";

    String cond_dotty =
        "infer/tests/codetoanalyze/c/frontend/conditional_operator/binary_operator.dot";

    ImmutableList<String> inferCmd =
        InferRunner.createCInferCommandFrontend(
            folder,
            cond_src);
    File newDotFile = InferRunner.runInferFrontend(inferCmd);
    assertThat(
        "In the capture of " + cond_src + " the dotty files should be the same.",
        newDotFile, dotFileEqualTo(cond_dotty));
  }

  @Test
  public void whenCaptureRunUnaryOperatorThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    String cond_src =
        "infer/tests/codetoanalyze/c/frontend/conditional_operator/unary_operator.c";

    String cond_dotty =
        "infer/tests/codetoanalyze/c/frontend/conditional_operator/unary_operator.dot";

    ImmutableList<String> inferCmd =
        InferRunner.createCInferCommandFrontend(
            folder,
            cond_src);
    File newDotFile = InferRunner.runInferFrontend(inferCmd);
    assertThat(
        "In the capture of " + cond_src + " the dotty files should be the same.",
        newDotFile, dotFileEqualTo(cond_dotty));
  }

  @Test
  public void whenCaptureRunArrayAccessThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    String cond_src =
        "infer/tests/codetoanalyze/c/frontend/conditional_operator/array_access.c";

    String cond_dotty =
        "infer/tests/codetoanalyze/c/frontend/conditional_operator/array_access.dot";

    ImmutableList<String> inferCmd =
        InferRunner.createCInferCommandFrontend(
            folder,
            cond_src);
    File newDotFile = InferRunner.runInferFrontend(inferCmd);
    assertThat(
        "In the capture of " + cond_src + " the dotty files should be the same.",
        newDotFile, dotFileEqualTo(cond_dotty));
  }

  @Test
  public void whenCaptureRunMemberAccessThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    String cond_src =
        "infer/tests/codetoanalyze/c/frontend/conditional_operator/member_access.c";

    String cond_dotty =
        "infer/tests/codetoanalyze/c/frontend/conditional_operator/member_access.dot";

    ImmutableList<String> inferCmd =
        InferRunner.createCInferCommandFrontend(
            folder,
            cond_src);
    File newDotFile = InferRunner.runInferFrontend(inferCmd);
    assertThat(
        "In the capture of " + cond_src + " the dotty files should be the same.",
        newDotFile, dotFileEqualTo(cond_dotty));
  }

  @Test
  public void whenCaptureRunPreincrementThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    String cond_src =
        "infer/tests/codetoanalyze/c/frontend/conditional_operator/preincrement.c";

    String cond_dotty =
        "infer/tests/codetoanalyze/c/frontend/conditional_operator/preincrement.dot";

    ImmutableList<String> inferCmd =
        InferRunner.createCInferCommandFrontend(
            folder,
            cond_src);
    File newDotFile = InferRunner.runInferFrontend(inferCmd);
    assertThat(
        "In the capture of " + cond_src + " the dotty files should be the same.",
        newDotFile, dotFileEqualTo(cond_dotty));
  }

  @Test
  public void whenCaptureRunFunctionCallThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    String cond_src =
        "infer/tests/codetoanalyze/c/frontend/conditional_operator/function_call.c";

    String cond_dotty =
        "infer/tests/codetoanalyze/c/frontend/conditional_operator/function_call.dot";

    ImmutableList<String> inferCmd =
        InferRunner.createCInferCommandFrontend(
            folder,
            cond_src);
    File newDotFile = InferRunner.runInferFrontend(inferCmd);
    assertThat(
        "In the capture of " + cond_src + " the dotty files should be the same.",
        newDotFile, dotFileEqualTo(cond_dotty));
  }

}
