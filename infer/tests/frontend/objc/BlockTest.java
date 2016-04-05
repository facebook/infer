/*
 * Copyright (c) 2013 - present Facebook, Inc.
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

import org.junit.Assume;
import org.junit.Rule;
import org.junit.Test;

import java.io.File;
import java.io.IOException;

import utils.DebuggableTemporaryFolder;
import utils.InferException;
import utils.InferRunner;


public class BlockTest {

  @Rule
  public DebuggableTemporaryFolder folder = new DebuggableTemporaryFolder();


  @Test
  public void whenCaptureRunOnPropertyThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {

    String block_src =
        "infer/tests/codetoanalyze/objc/frontend/" +
            "block/block.m";

    String block_dotty =
        "infer/tests/codetoanalyze/objc/frontend/" +
            "block/block.dot";

    ImmutableList<String> inferCmd =
        InferRunner.createObjCInferCommandFrontend(folder, block_src);
    File newDotFile = InferRunner.runInferFrontend(inferCmd);
    assertThat(
        "In the capture of " + block_src +
            " the dotty files should be the same.",
        newDotFile, dotFileEqualTo(block_dotty));
  }

  @Test
  public void whenCaptureRunOnBlockVarThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {

    String block_src =
        "infer/tests/codetoanalyze/objc/frontend/block/BlockVar.m";

    String block_dotty =
        "infer/tests/codetoanalyze/objc/frontend/block/BlockVar.dot";

    ImmutableList<String> inferCmd =
        InferRunner.createObjCInferCommandFrontend(folder, block_src);
    File newDotFile = InferRunner.runInferFrontend(inferCmd);
    assertThat(
        "In the capture of " + block_src +
            " the dotty files should be the same.",
        newDotFile, dotFileEqualTo(block_dotty));
  }

  @Test
  public void whenCaptureRunOnBlockReleaseThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {

    String block_src =
        "infer/tests/codetoanalyze/objc/frontend/block/block_release.m";

    String block_dotty =
        "infer/tests/codetoanalyze/objc/frontend/block/block_release.dot";

    ImmutableList<String> inferCmd =
        InferRunner.createiOSInferCommandFrontend(folder, block_src);
    File newDotFile = InferRunner.runInferFrontend(inferCmd);
    assertThat(
        "In the capture of " + block_src +
            " the dotty files should be the same.",
        newDotFile, dotFileEqualTo(block_dotty));
  }

  @Test
  public void whenCaptureRunOnBlockNoArgsThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {

    String block_src =
        "infer/tests/codetoanalyze/objc/frontend/block/block_no_args.m";

    String block_dotty =
        "infer/tests/codetoanalyze/objc/frontend/block/block_no_args.dot";

    ImmutableList<String> inferCmd =
        InferRunner.createObjCInferCommandFrontend(folder, block_src);
    File newDotFile = InferRunner.runInferFrontend(inferCmd);
    assertThat(
        "In the capture of " + block_src +
            " the dotty files should be the same.",
        newDotFile, dotFileEqualTo(block_dotty));
  }

  @Test
  public void whenCaptureRunOnStaticThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {

    String block_src =
        "infer/tests/codetoanalyze/objc/frontend/block/static.m";

    String block_dotty =
        "infer/tests/codetoanalyze/objc/frontend/block/static.dot";

    ImmutableList<String> inferCmd =
        InferRunner.createiOSInferCommandFrontend(folder, block_src);
    File newDotFile = InferRunner.runInferFrontend(inferCmd);
    assertThat(
        "In the capture of " + block_src +
            " the dotty files should be the same.",
        newDotFile, dotFileEqualTo(block_dotty));
  }
  @Test
  public void whenCaptureRunOnRetainCycleThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {

    String block_src =
        "infer/tests/codetoanalyze/objc/frontend/block/retain_cycle.m";

    String block_dotty =
        "infer/tests/codetoanalyze/objc/frontend/block/retain_cycle.dot";

    ImmutableList<String> inferCmd =
        InferRunner.createiOSInferCommandFrontend(folder, block_src);
    File newDotFile = InferRunner.runInferFrontend(inferCmd);
    assertThat(
        "In the capture of " + block_src +
            " the dotty files should be the same.",
        newDotFile, dotFileEqualTo(block_dotty));
  }

    @Test
    public void whenCaptureRunOnDispatchThenDotFilesAreTheSame()
    throws InterruptedException, IOException, InferException {

        String block_src =
        "infer/tests/codetoanalyze/objc/frontend/block/dispatch.m";

        String block_dotty =
        "infer/tests/codetoanalyze/objc/frontend/block/dispatch.dot";

        ImmutableList<String> inferCmd =
        InferRunner.createiOSInferCommandFrontend(folder, block_src);
        File newDotFile = InferRunner.runInferFrontend(inferCmd);
        assertThat(
                   "In the capture of " + block_src +
                   " the dotty files should be the same.",
                   newDotFile, dotFileEqualTo(block_dotty));
    }

  @Test
  public void whenCaptureRunOnDispatch_exampleThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {

    String block_src =
        "infer/tests/codetoanalyze/objc/frontend/block/dispatch_examples.m";

    String block_dotty =
        "infer/tests/codetoanalyze/objc/frontend/block/dispatch_examples.dot";

    ImmutableList<String> inferCmd =
        InferRunner.createiOSInferCommandFrontend(folder, block_src);
    File newDotFile = InferRunner.runInferFrontend(inferCmd);
    assertThat(
        "In the capture of " + block_src +
            " the dotty files should be the same.",
        newDotFile, dotFileEqualTo(block_dotty));
  }

  @Test
  public void whenCaptureRunOnBlockit_exampleThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {

    String block_src =
        "infer/tests/codetoanalyze/objc/frontend/block/block-it.m";

    String block_dotty =
        "infer/tests/codetoanalyze/objc/frontend/block/block-it.dot";

    ImmutableList<String> inferCmd =
        InferRunner.createiOSInferCommandFrontend(folder, block_src);
    File newDotFile = InferRunner.runInferFrontend(inferCmd);
    assertThat(
        "In the capture of " + block_src +
            " the dotty files should be the same.",
        newDotFile, dotFileEqualTo(block_dotty));
  }


}
