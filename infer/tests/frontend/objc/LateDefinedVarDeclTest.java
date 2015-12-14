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

public class LateDefinedVarDeclTest {

  @Rule
  public DebuggableTemporaryFolder folder = new DebuggableTemporaryFolder();


  @Test
  public void whenCaptureRunLateDefinedStaticVarThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    String src_file =
        "infer/tests/" +
            "codetoanalyze/objc/frontend/vardecl/aclass.m";

    String dot_file =
        "infer/tests/" +
            "codetoanalyze/objc/frontend/vardecl/aclass.dot";

    ImmutableList<String> inferCmd =
        InferRunner.createObjCInferCommandFrontend(folder, src_file);
    File newDotFile = InferRunner.runInferFrontend(inferCmd);
    assertThat(
        "In the capture of " + src_file +
            " the dotty files should be the same.",
        newDotFile, dotFileEqualTo(dot_file));
  }

  @Test
  public void whenCaptureRunLateDefinedVarThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    String src_file =
        "infer/tests/" +
            "codetoanalyze/objc/frontend/vardecl/aclass_2.m";

    String dot_file =
        "infer/tests/" +
            "codetoanalyze/objc/frontend/vardecl/aclass_2.dot";

    ImmutableList<String> inferCmd =
        InferRunner.createObjCInferCommandFrontend(folder, src_file);
    File newDotFile = InferRunner.runInferFrontend(inferCmd);
    assertThat(
        "In the capture of " + src_file +
            " the dotty files should be the same.",
        newDotFile, dotFileEqualTo(dot_file));
  }

  @Test
  public void whenCaptureRunOnFunctionDeclWithOrderedVarDeclsThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    String src_file =
        "infer/tests/" +
            "codetoanalyze/objc/frontend/vardecl/last_af.m";

    String dot_file =
        "infer/tests/" +
            "codetoanalyze/objc/frontend/vardecl/last_af.dot";

    ImmutableList<String> inferCmd =
        InferRunner.createObjCInferCommandFrontend(folder, src_file);
    File newDotFile = InferRunner.runInferFrontend(inferCmd);
    assertThat(
        "In the capture of " + src_file +
            " the dotty files should be the same.",
        newDotFile, dotFileEqualTo(dot_file));
  }
}
