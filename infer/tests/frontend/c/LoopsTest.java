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

public class LoopsTest {

  @Rule
  public DebuggableTemporaryFolder folder = new DebuggableTemporaryFolder();


  @Test
  public void whenCaptureRunOnWhileThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    String while_expr =
        "infer/tests/codetoanalyze/c/frontend/loops/while.c";

    String while_dotty =
        "infer/tests/codetoanalyze/c/frontend/loops/while.dot";

    ImmutableList<String> inferCmd =
        InferRunner.createCInferCommandFrontend(folder, while_expr);
    File newDotFile = InferRunner.runInferFrontend(inferCmd);
    assertThat(
        "In the capture of " + while_expr +
            " the dotty files should be the same.",
        newDotFile, dotFileEqualTo(while_dotty));
  }

  @Test
  public void whenCaptureRunOnWhile_nestedThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    String while_expr =
        "infer/tests/codetoanalyze/c/frontend/loops/while_nested.c";

    String while_dotty =
        "infer/tests/codetoanalyze/c/frontend/loops/while_nested.dot";

    ImmutableList<String> inferCmd =
        InferRunner.createCInferCommandFrontend(folder, while_expr);
    File newDotFile = InferRunner.runInferFrontend(inferCmd);
    assertThat(
        "In the capture of " + while_expr +
            " the dotty files should be the same.",
        newDotFile, dotFileEqualTo(while_dotty));
  }

  @Test
  public void whenCaptureRunOnWhile_side_effectsThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    String while_expr =
        "infer/tests/codetoanalyze/" +
            "c/frontend/loops/while_condition_side_effects.c";

    String while_dotty =
        "infer/tests/codetoanalyze/" +
            "c/frontend/loops/while_condition_side_effects.dot";

    ImmutableList<String> inferCmd =
        InferRunner.createCInferCommandFrontend(folder, while_expr);
    File newDotFile = InferRunner.runInferFrontend(inferCmd);
    assertThat(
        "In the capture of " + while_expr +
            " the dotty files should be the same.",
        newDotFile, dotFileEqualTo(while_dotty));
  }

  @Test
  public void whenCaptureRunOnWhile_no_bodyThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    String while_expr =
        "infer/tests/codetoanalyze/c/frontend/loops/while_no_body.c";

    String while_dotty =
        "infer/tests/codetoanalyze/c/frontend/loops/while_no_body.dot";

    ImmutableList<String> inferCmd =
        InferRunner.createCInferCommandFrontend(folder, while_expr);
    File newDotFile = InferRunner.runInferFrontend(inferCmd);
    assertThat(
        "In the capture of " + while_expr +
            " the dotty files should be the same.",
        newDotFile, dotFileEqualTo(while_dotty));
  }

  @Test
  public void whenCaptureRunOnForside_effectsThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    String for_expr =
        "infer/tests/codetoanalyze/" +
            "c/frontend/loops/for_condition_side_effects.c";

    String for_dotty =
        "infer/tests/codetoanalyze/" +
            "c/frontend/loops/for_condition_side_effects.dot";

    ImmutableList<String> inferCmd =
        InferRunner.createCInferCommandFrontend(folder, for_expr);
    File newDotFile = InferRunner.runInferFrontend(inferCmd);
    assertThat(
        "In the capture of " + for_expr +
            " the dotty files should be the same.",
        newDotFile, dotFileEqualTo(for_dotty));
  }

  @Test
  public void whenCaptureRunOnFor_nestedThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    String for_expr =
        "infer/tests/codetoanalyze/c/frontend/loops/for_nested.c";

    String for_dotty =
        "infer/tests/codetoanalyze/c/frontend/loops/for_nested.dot";

    ImmutableList<String> inferCmd =
        InferRunner.createCInferCommandFrontend(folder, for_expr);
    File newDotFile = InferRunner.runInferFrontend(inferCmd);
    assertThat(
        "In the capture of " + for_expr +
            " the dotty files should be the same.",
        newDotFile, dotFileEqualTo(for_dotty));
  }

  @Test
  public void whenCaptureRunOnFor_no_conditionThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    String for_expr =
        "infer/tests/codetoanalyze/" +
            "c/frontend/loops/for_no_condition.c";

    String for_dotty =
        "infer/tests/codetoanalyze/" +
            "c/frontend/loops/for_no_condition.dot";

    ImmutableList<String> inferCmd =
        InferRunner.createCInferCommandFrontend(folder, for_expr);
    File newDotFile = InferRunner.runInferFrontend(inferCmd);
    assertThat(
        "In the capture of " + for_expr +
            " the dotty files should be the same.",
        newDotFile, dotFileEqualTo(for_dotty));
  }

  @Test
  public void whenCaptureRunOnFor_no_conditionincrThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    String for_expr =
        "infer/tests/codetoanalyze/" +
            "c/frontend/loops/for_no_condition_incr.c";

    String for_dotty =
        "infer/tests/codetoanalyze/" +
            "c/frontend/loops/for_no_condition_incr.dot";

    ImmutableList<String> inferCmd =
        InferRunner.createCInferCommandFrontend(folder, for_expr);
    File newDotFile = InferRunner.runInferFrontend(inferCmd);
    assertThat(
        "In the capture of " + for_expr +
            " the dotty files should be the same.",
        newDotFile, dotFileEqualTo(for_dotty));
  }

  @Test
  public void whenCaptureRunOnFor_emptyThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    String for_expr =
        "infer/tests/codetoanalyze/" +
            "c/frontend/loops/for_no_condition_incr_body.c";

    String for_dotty =
        "infer/tests/codetoanalyze/" +
            "c/frontend/loops/for_no_condition_incr_body.dot";

    ImmutableList<String> inferCmd =
        InferRunner.createCInferCommandFrontend(folder, for_expr);
    File newDotFile = InferRunner.runInferFrontend(inferCmd);
    assertThat(
        "In the capture of " + for_expr +
            " the dotty files should be the same.",
        newDotFile, dotFileEqualTo(for_dotty));
  }

  @Test
  public void whenCaptureRunOnFor_only_bodyThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    String for_expr =
        "infer/tests/codetoanalyze/c/frontend/loops/for_only_body.c";

    String for_dotty =
        "infer/tests/codetoanalyze/c/frontend/loops/for_only_body.dot";

    ImmutableList<String> inferCmd =
        InferRunner.createCInferCommandFrontend(folder, for_expr);
    File newDotFile = InferRunner.runInferFrontend(inferCmd);
    assertThat(
        "In the capture of " + for_expr +
            " the dotty files should be the same.",
        newDotFile, dotFileEqualTo(for_dotty));
  }

  @Test
  public void whenCaptureRunOnFor_simpleThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    String for_expr =
        "infer/tests/codetoanalyze/c/frontend/loops/for_simple.c";

    String for_dotty =
        "infer/tests/codetoanalyze/c/frontend/loops/for_simple.dot";

    ImmutableList<String> inferCmd =
        InferRunner.createCInferCommandFrontend(folder, for_expr);
    File newDotFile = InferRunner.runInferFrontend(inferCmd);
    assertThat(
        "In the capture of " + for_expr +
            " the dotty files should be the same.",
        newDotFile, dotFileEqualTo(for_dotty));
  }

  @Test
  public void whenCaptureRunOnFor_while_nestedThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    String for_expr =
        "infer/tests/codetoanalyze/" +
            "c/frontend/loops/for_while_nested.c";

    String for_dotty =
        "infer/tests/codetoanalyze/" +
            "c/frontend/loops/for_while_nested.dot";

    ImmutableList<String> inferCmd =
        InferRunner.createCInferCommandFrontend(folder, for_expr);
    File newDotFile = InferRunner.runInferFrontend(inferCmd);
    assertThat(
        "In the capture of " + for_expr +
            " the dotty files should be the same.",
        newDotFile, dotFileEqualTo(for_dotty));
  }

  @Test
  public void whenCaptureRunOnDo_whileThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    String do_while_expr =
        "infer/tests/codetoanalyze/c/frontend/loops/do_while.c";

    String do_while_dotty =
        "infer/tests/codetoanalyze/c/frontend/loops/do_while.dot";

    ImmutableList<String> inferCmd =
        InferRunner.createCInferCommandFrontend(folder, do_while_expr);
    File newDotFile = InferRunner.runInferFrontend(inferCmd);
    assertThat(
        "In the capture of " + do_while_expr +
            " the dotty files should be the same.",
        newDotFile, dotFileEqualTo(do_while_dotty));
  }

  @Test
  public void whenCaptureRunOnDo_while_side_effectsThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    String do_while_expr =
        "infer/tests/codetoanalyze/" +
            "c/frontend/loops/do_while_condition_side_effects.c";

    String do_while_dotty =
        "infer/tests/codetoanalyze/" +
            "c/frontend/loops/do_while_condition_side_effects.dot";

    ImmutableList<String> inferCmd =
        InferRunner.createCInferCommandFrontend(folder, do_while_expr);
    File newDotFile = InferRunner.runInferFrontend(inferCmd);
    assertThat(
        "In the capture of " + do_while_expr +
            " the dotty files should be the same.",
        newDotFile, dotFileEqualTo(do_while_dotty));
  }

  @Test
  public void whenCaptureRunOnDo_while_nestedThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    String do_while_expr =
        "infer/tests/codetoanalyze/c/frontend/loops/do_while_nested.c";

    String do_while_dotty =
        "infer/tests/codetoanalyze/" +
            "c/frontend/loops/do_while_nested.dot";

    ImmutableList<String> inferCmd =
        InferRunner.createCInferCommandFrontend(folder, do_while_expr);
    File newDotFile = InferRunner.runInferFrontend(inferCmd);
    assertThat(
        "In the capture of " + do_while_expr +
            " the dotty files should be the same.",
        newDotFile, dotFileEqualTo(do_while_dotty));
  }

  @Test
  public void whenCaptureWhile_with_continue_and_breakThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    String while_with_continue_and_break_expr =
        "infer/tests/codetoanalyze/" +
            "c/frontend/loops/while_with_continue_and_break.c";

    String while_with_continue_and_break_dotty =
        "infer/tests/codetoanalyze/" +
            "c/frontend/loops/while_with_continue_and_break.dot";

    ImmutableList<String> inferCmd = InferRunner.createCInferCommandFrontend(
        folder, while_with_continue_and_break_expr);
    File newDotFile = InferRunner.runInferFrontend(inferCmd);
    assertThat(
        "In the capture of " + while_with_continue_and_break_expr +
            " the dotty files should be the same.",
        newDotFile, dotFileEqualTo(while_with_continue_and_break_dotty));
  }
}
