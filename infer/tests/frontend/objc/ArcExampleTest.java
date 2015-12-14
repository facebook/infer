/*
 * Copyright (c) 2014 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package frontend.objc;

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


public class ArcExampleTest {

  @Rule
  public DebuggableTemporaryFolder folder = new DebuggableTemporaryFolder();


  @Test
  public void whenCaptureRunOnPropertyThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {

    String block_src = "infer/tests/codetoanalyze/objc/errors/memory_leaks_benchmark/ArcExample.m";

    String block_dotty =
        "infer/tests/codetoanalyze/objc/errors/memory_leaks_benchmark/ArcExample.dot";

    ImmutableList<String> inferCmd =
        InferRunner.createObjCInferCommandFrontendArc(folder, block_src);
    File newDotFile = InferRunner.runInferFrontend(inferCmd);
    assertThat(
        "In the capture of " + block_src +
            " the dotty files should be the same.",
        newDotFile, dotFileEqualTo(block_dotty));
  }

  @Test
  public void whenCaptureInitlistThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {

    String block_src = "infer/tests/codetoanalyze/objc/frontend/vardecl/initlist.m";

    String block_dotty =
        "infer/tests/codetoanalyze/objc/frontend/vardecl/initlist.dot";

    ImmutableList<String> inferCmd =
        InferRunner.createObjCInferCommandFrontendArc(folder, block_src);
    File newDotFile = InferRunner.runInferFrontend(inferCmd);
    assertThat(
        "In the capture of " + block_src +
            " the dotty files should be the same.",
        newDotFile, dotFileEqualTo(block_dotty));
  }

  @Test
  public void whenCaptureRunOnArcExampleThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {

    String arc_src = "infer/tests/codetoanalyze/objc/errors/memory_leaks_benchmark/arc_methods.m";

    String arc_dotty =
        "infer/tests/codetoanalyze/objc/errors/memory_leaks_benchmark/arc_methods.dot";

    ImmutableList<String> inferCmd =
        InferRunner.createObjCInferCommandFrontendArc(folder, arc_src);
    File newDotFile = InferRunner.runInferFrontend(inferCmd);
    assertThat(
        "In the capture of " + arc_src + " the dotty files should be the same.",
        newDotFile, dotFileEqualTo(arc_dotty));
  }
}
