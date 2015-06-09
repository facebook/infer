/*
 * Copyright (c) 2013- Facebook.
 * All rights reserved.
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

public class StringTest {

  @Rule
  public DebuggableTemporaryFolder folder = new DebuggableTemporaryFolder();


  @Test
  public void whenCaptureRunOnStringLiteralThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    String string_expr =
        "infer/tests/codetoanalyze/" +
            "objc/frontend/strings/string_literal.m";

    String string_dotty =
        "infer/tests/codetoanalyze/" +
            "objc/frontend/strings/string_literal.dot";

    ImmutableList<String> inferCmd =
        InferRunner.createObjCInferCommandFrontend(folder, string_expr);
    File newDotFile = InferRunner.runInferFrontend(inferCmd);
    assertThat(
        "In the capture of " + string_expr +
            " the dotty files should be the same.",
        newDotFile, dotFileEqualTo(string_dotty));
  }

  @Test
  public void whenCaptureRunOnGlobalStringLiteralThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    String string_expr =
        "infer/tests/codetoanalyze/" +
            "objc/frontend/strings/global_string_literal.m";

    String string_dotty =
        "infer/tests/codetoanalyze/" +
            "objc/frontend/strings/global_string_literal.dot";

    ImmutableList<String> inferCmd =
        InferRunner.createObjCInferCommandFrontend(folder, string_expr);
    File newDotFile = InferRunner.runInferFrontend(inferCmd);
    assertThat(
        "In the capture of " + string_expr +
            " the dotty files should be the same.",
        newDotFile, dotFileEqualTo(string_dotty));
  }
}
