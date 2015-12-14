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


public class StaticSelf {

  @Rule
  public DebuggableTemporaryFolder folder = new DebuggableTemporaryFolder();


  @Test
  public void whenCaptureRunOnStaticThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    String property_expr =
        "infer/tests/codetoanalyze/" +
            "objc/frontend/self_static/static.m";

    String property_dotty =
        "infer/tests/codetoanalyze/" +
            "objc/frontend/self_static/static.dot";

    ImmutableList<String> inferCmd =
        InferRunner.createObjCInferCommandFrontend(
            folder,
            property_expr);
    File newDotFile = InferRunner.runInferFrontend(inferCmd);
    assertThat(
        "In the capture of " + property_expr +
            " the dotty files should be the same.",
        newDotFile, dotFileEqualTo(property_dotty));
  }

  @Test
  public void whenCaptureRunOnSuperTestThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    String expr = "infer/tests/codetoanalyze/objc/errors/field_superclass/SuperExample.m";

    String dotty = "infer/tests/codetoanalyze/objc/errors/field_superclass/SuperExample.dot";

    ImmutableList<String> inferCmd =
        InferRunner.createObjCInferCommandFrontend(
            folder,
            expr);
    File newDotFile = InferRunner.runInferFrontend(inferCmd);
    assertThat(
        "In the capture of " + expr +
            " the dotty files should be the same.",
        newDotFile, dotFileEqualTo(dotty));
  }

  @Test
  public void whenCaptureRunOnSelfThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    String expr = "infer/tests/codetoanalyze/objc/frontend/self_static/Self.m";

    String dotty = "infer/tests/codetoanalyze/objc/frontend/self_static/Self.dot";

    ImmutableList<String> inferCmd =
        InferRunner.createObjCInferCommandFrontend(
            folder,
            expr);
    File newDotFile = InferRunner.runInferFrontend(inferCmd);
    assertThat(
        "In the capture of " + expr +
            " the dotty files should be the same.",
        newDotFile, dotFileEqualTo(dotty));
  }


}
