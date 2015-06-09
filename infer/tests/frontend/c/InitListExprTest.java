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

public class InitListExprTest {
  @Rule
  public DebuggableTemporaryFolder folder = new DebuggableTemporaryFolder();

  @Test
  public void whenCaptureRunOnArrayInitListExprThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    String plus_expr =
        "infer/tests/codetoanalyze/" +
            "c/frontend/initialization/array_initlistexpr.c";

    String plus_expr_dotty =
        "infer/tests/codetoanalyze/" +
            "c/frontend/initialization/array_initlistexpr.dot";

    ImmutableList<String> inferCmd =
        InferRunner.createCInferCommandFrontend(folder, plus_expr);
    File newDotFile = InferRunner.runInferFrontend(inferCmd);
    assertThat(
        "In the capture of " + plus_expr +
            " the dotty files should be the same.",
        newDotFile, dotFileEqualTo(plus_expr_dotty));
  }

  @Test
  public void whenCaptureRunOnStructInitListExprThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    String plus_expr =
        "infer/tests/codetoanalyze/" +
            "c/frontend/initialization/struct_initlistexpr.c";

    String plus_expr_dotty =
        "infer/tests/codetoanalyze/" +
            "c/frontend/initialization/struct_initlistexpr.dot";

    ImmutableList<String> inferCmd =
        InferRunner.createCInferCommandFrontend(folder, plus_expr);
    File newDotFile = InferRunner.runInferFrontend(inferCmd);
    assertThat(
        "In the capture of " + plus_expr +
            " the dotty files should be the same.",
        newDotFile, dotFileEqualTo(plus_expr_dotty));
  }
}
