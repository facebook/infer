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

public class GnuexprTest {

  @Rule
  public DebuggableTemporaryFolder folder = new DebuggableTemporaryFolder();


  @Test
  public void whenCaptureRunOnWhileThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    String gnu_expr =
        "infer/tests/codetoanalyze/" +
            "c/frontend/nestedoperators/gnuexpr.c";

    String gnu_dotty =
        "infer/tests/codetoanalyze/" +
            "c/frontend/nestedoperators/gnuexpr.dot";

    ImmutableList<String> inferCmd =
        InferRunner.createCInferCommandFrontend(folder, gnu_expr);
    File newDotFile = InferRunner.runInferFrontend(inferCmd);
    assertThat(
        "In the capture of " + gnu_expr +
            " the dotty files should be the same.",
        newDotFile, dotFileEqualTo(gnu_dotty));
  }

}
