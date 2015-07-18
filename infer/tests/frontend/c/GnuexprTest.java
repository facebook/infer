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
