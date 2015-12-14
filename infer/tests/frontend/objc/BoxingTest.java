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

public class BoxingTest {

  @Rule
  public DebuggableTemporaryFolder folder = new DebuggableTemporaryFolder();


  @Test
  public void whenCaptureRunOnBoxingThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    String boxing_expr =
        "infer/tests/codetoanalyze/" +
            "objc/frontend/boxing/Boxing.m";

    String boxing_dotty =
        "infer/tests/codetoanalyze/" +
            "objc/frontend/boxing/Boxing.dot";

    ImmutableList<String> inferCmd =
        InferRunner.createObjCInferCommandFrontend(folder, boxing_expr);
    File newDotFile = InferRunner.runInferFrontend(inferCmd);
    assertThat(
        "In the capture of " + boxing_expr +
            " the dotty files should be the same. " +
            "In each procedure the translation of the boxing " +
            "syntactic sugar is the same " +
            "as the tranlation of the underlying method call.",
        newDotFile, dotFileEqualTo(boxing_dotty));
  }

  @Test
  public void whenCaptureRunOnArray_literalThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    String arr_literal_expr =
        "infer/tests/codetoanalyze/" +
            "objc/frontend/boxing/array_literal.c";

    String arr_literal_dotty =
        "infer/tests/codetoanalyze/" +
            "objc/frontend/boxing/array_literal.dot";

    ImmutableList<String> inferCmd =
        InferRunner.createObjCInferCommandFrontend(
            folder,
            arr_literal_expr);
    File newDotFile = InferRunner.runInferFrontend(inferCmd);
    assertThat(
        "In the capture of " + arr_literal_expr +
            " the dotty files should be the same. " +
            "In each procedure the translation of the boxing " +
            "syntactic sugar is the same " +
            "as the tranlation of the underlying method call.",
        newDotFile, dotFileEqualTo(arr_literal_dotty));
  }

  @Test
  public void whenCaptureRunOnDict_literalThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    String dict_literal_expr =
        "infer/tests/codetoanalyze/" +
            "objc/frontend/boxing/dict_literal.c";

    String dict_literal_dotty =
        "infer/tests/codetoanalyze/" +
            "objc/frontend/boxing/dict_literal.dot";

    ImmutableList<String> inferCmd =
        InferRunner.createObjCInferCommandFrontend(
            folder,
            dict_literal_expr);
    File newDotFile = InferRunner.runInferFrontend(inferCmd);
    assertThat(
        "In the capture of " + dict_literal_expr +
            " the dotty files should be the same. " +
            "In each procedure the translation of the boxing " +
            "syntactic sugar is the same " +
            "as the tranlation of the underlying method call.",
        newDotFile, dotFileEqualTo(dict_literal_dotty));
  }

  @Test
  public void whenCaptureRunOnString_literalThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    String string_literal_expr =
        "infer/tests/codetoanalyze/" +
            "objc/frontend/boxing/string_literal.c";

    String string_literal_dotty =
        "infer/tests/codetoanalyze/" +
            "objc/frontend/boxing/string_literal.dot";

    ImmutableList<String> inferCmd =
        InferRunner.createObjCInferCommandFrontend(
            folder,
            string_literal_expr);
    File newDotFile = InferRunner.runInferFrontend(inferCmd);
    assertThat(
        "In the capture of " + string_literal_expr +
            " the dotty files should be the same. " +
            "In each procedure the translation of the boxing " +
            "syntactic sugar is the same " +
            "as the tranlation of the underlying method call.",
        newDotFile, dotFileEqualTo(string_literal_dotty));
  }

  @Test
  public void whenCaptureRunOnForCollection_ThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    String array_expr =
        "infer/tests/codetoanalyze/objc/frontend/boxing/array.m";

    String array_dotty =
        "infer/tests/codetoanalyze/objc/frontend/boxing/array.dot";

    ImmutableList<String> inferCmd =
        InferRunner.createObjCInferCommandFrontend(folder, array_expr);
    File newDotFile = InferRunner.runInferFrontend(inferCmd);
    assertThat(
        "In the capture of " + array_expr +
            " the dotty files should be the same.",
        newDotFile, dotFileEqualTo(array_dotty));
  }

}
