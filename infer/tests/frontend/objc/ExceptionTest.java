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


public class ExceptionTest {

  @Rule
  public DebuggableTemporaryFolder folder = new DebuggableTemporaryFolder();


  @Test
  public void whenCaptureRunOnTestThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {

    String exception_src =
        "infer/tests/codetoanalyze/objc/frontend/exceptions/ExceptionExample.m";

    String exception_dotty =
        "infer/tests/codetoanalyze/objc/frontend/exceptions/ExceptionExample.dot";

    ImmutableList<String> inferCmd =
        InferRunner.createObjCInferCommandFrontend(folder, exception_src);
    File newDotFile = InferRunner.runInferFrontend(inferCmd);
    assertThat(
        "In the capture of " + exception_src +
            " the dotty files should be the same.",
        newDotFile, dotFileEqualTo(exception_dotty));
  }
}
