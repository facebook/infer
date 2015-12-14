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

public class SubclassTest {

  @Rule
  public DebuggableTemporaryFolder folder = new DebuggableTemporaryFolder();


  @Test
  public void whenCaptureRunOnStringLiteralThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    String myClass_expr =
        "infer/tests/codetoanalyze/" +
            "objc/frontend/subclass/MyClass.m";

    String string_dotty =
        "infer/tests/codetoanalyze/" +
            "objc/frontend/subclass/MyClass.dot";

    ImmutableList<String> inferCmd =
        InferRunner.createObjCInferCommandFrontend(
            folder,
            myClass_expr);
    File newDotFile = InferRunner.runInferFrontend(inferCmd);
    assertThat(
        "In the capture of " + myClass_expr +
            " the dotty files should be the same.",
        newDotFile, dotFileEqualTo(string_dotty));
  }

  @Test
  public void whenCaptureRunOnGlobalStringLiteralThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    String mySubClass_expr =
        "infer/tests/codetoanalyze/" +
            "objc/frontend/subclass/MySubClass.m";

    String string_dotty =
        "infer/tests/codetoanalyze/" +
            "objc/frontend/subclass/MySubClass.dot";

    ImmutableList<String> inferCmd =
        InferRunner.createObjCInferCommandFrontend(
            folder,
            mySubClass_expr);
    File newDotFile = InferRunner.runInferFrontend(inferCmd);
    assertThat(
        "In the capture of " + mySubClass_expr +
            " the dotty files should be the same.",
        newDotFile, dotFileEqualTo(string_dotty));
  }

  @Test
  public void whenCaptureRunOnMainThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    String mySubClass_expr = "infer/tests/codetoanalyze/objc/frontend/subclass/main.c";

    String string_dotty = "infer/tests/codetoanalyze/objc/frontend/subclass/main.dot";

    ImmutableList<String> inferCmd =
        InferRunner.createObjCInferCommandFrontend(
            folder,
            mySubClass_expr);
    File newDotFile = InferRunner.runInferFrontend(inferCmd);
    assertThat(
        "In the capture of " + mySubClass_expr +
            " the dotty files should be the same.",
        newDotFile, dotFileEqualTo(string_dotty));
  }


}
