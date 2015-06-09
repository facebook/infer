/*
 * Copyright (c) 2013- Facebook.
 * All rights reserved.
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

public class ArithmeticExpTest {

  @Rule
  public DebuggableTemporaryFolder folder = new DebuggableTemporaryFolder();


  @Test
  public void whenCaptureRunOnPlus_exprThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    String plus_expr =
        "infer/tests/" +
            "codetoanalyze/c/frontend/arithmetic/plus_expr.c";

    String plus_expr_dotty =
        "infer/tests/" +
            "codetoanalyze/c/frontend/arithmetic/plus_expr.dot";

    ImmutableList<String> inferCmd =
        InferRunner.createCInferCommandFrontend(folder, plus_expr);
    File newDotFile = InferRunner.runInferFrontend(inferCmd);
    assertThat(
        "In the capture of " + plus_expr +
            " the dotty files should be the same.",
        newDotFile, dotFileEqualTo(plus_expr_dotty));
  }

  @Test
  public void whenCaptureRunOnCompound_assignmentThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    String compound_assignment_expr =
        "infer/tests/codetoanalyze/c/frontend/" +
            "arithmetic/compound_assignment.c";

    String compound_assignment_dotty =
        "infer/tests/codetoanalyze/c/frontend/" +
            "arithmetic/compound_assignment.dot";

    ImmutableList<String> inferCmd =
        InferRunner.createCInferCommandFrontend(
            folder,
            compound_assignment_expr);
    File newDotFile = InferRunner.runInferFrontend(inferCmd);
    assertThat(
        "In the capture of " + compound_assignment_expr +
            " the dotty files should be the same.",
        newDotFile, dotFileEqualTo(compound_assignment_dotty));
  }

  @Test
  public void whenCaptureRunOnUnaryThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    String unary_expr =
        "infer/tests/" +
            "codetoanalyze/c/frontend/arithmetic/unary.c";

    String unary_dotty =
        "infer/tests/" +
            "codetoanalyze/c/frontend/arithmetic/unary.dot";

    ImmutableList<String> inferCmd =
        InferRunner.createCInferCommandFrontend(folder, unary_expr);
    File newDotFile = InferRunner.runInferFrontend(inferCmd);
    assertThat(
        "In the capture of " + unary_expr +
            " the dotty files should be the same.",
        newDotFile, dotFileEqualTo(unary_dotty));
  }

  @Test
  public void whenCaptureRunOnIntConstThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    String int_const_expr =
        "infer/tests/" +
            "codetoanalyze/c/frontend/arithmetic/int_const.c";

    String int_const_dotty =
        "infer/tests/" +
            "codetoanalyze/c/frontend/arithmetic/int_const.dot";

    ImmutableList<String> inferCmd =
        InferRunner.createCInferCommandFrontend(folder, int_const_expr);
    File newDotFile = InferRunner.runInferFrontend(inferCmd);
    assertThat(
        "In the capture of " + int_const_expr +
            " the dotty files should be the same.",
        newDotFile, dotFileEqualTo(int_const_dotty));
  }

}
