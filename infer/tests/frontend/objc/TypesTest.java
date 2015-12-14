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

import org.junit.Rule;
import org.junit.Test;

import java.io.File;
import java.io.IOException;

import utils.DebuggableTemporaryFolder;
import utils.InferException;
import utils.InferRunner;


public class TypesTest {

  @Rule
  public DebuggableTemporaryFolder folder = new DebuggableTemporaryFolder();


  @Test
  public void whenCaptureRunOnTestLoopThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {

    String block_src =
        "infer/tests/codetoanalyze/objc/frontend/" +
            "types/testloop.m";

    String block_dotty =
        "infer/tests/codetoanalyze/objc/frontend/" +
            "types/testloop.dot";

    ImmutableList<String> inferCmd =
        InferRunner.createObjCInferCommandFrontend(folder, block_src);
    File newDotFile = InferRunner.runInferFrontend(inferCmd);
    assertThat(
        "In the capture of " + block_src +
            " the dotty files should be the same.",
        newDotFile, dotFileEqualTo(block_dotty));
  }

    @Test
    public void whenCaptureRunOnAttributesThenDotFilesAreTheSame()
    throws InterruptedException, IOException, InferException {

        String block_src =
        "infer/tests/codetoanalyze/objc/frontend/" +
        "types/attributes.m";

        String block_dotty =
        "infer/tests/codetoanalyze/objc/frontend/" +
        "types/attributes.dot";

        ImmutableList<String> inferCmd =
        InferRunner.createObjCInferCommandFrontendArc(folder, block_src);
        File newDotFile = InferRunner.runInferFrontend(inferCmd);
        assertThat(
                   "In the capture of " + block_src +
                   " the dotty files should be the same.",
                   newDotFile, dotFileEqualTo(block_dotty));
    }

  @Test
  public void whenCaptureRunOnVoidCall()
      throws InterruptedException, IOException, InferException {

    String block_src =
        "infer/tests/codetoanalyze/objc/frontend/" +
            "types/void_call.m";

    String block_dotty =
        "infer/tests/codetoanalyze/objc/frontend/" +
            "types/void_call.dot";

    ImmutableList<String> inferCmd =
        InferRunner.createObjCInferCommandFrontendArc(folder, block_src);
    File newDotFile = InferRunner.runInferFrontend(inferCmd);
    assertThat(
        "In the capture of " + block_src +
            " the dotty files should be the same.",
        newDotFile, dotFileEqualTo(block_dotty));
  }
}
