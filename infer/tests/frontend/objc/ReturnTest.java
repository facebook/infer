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


public class ReturnTest {

  @Rule
  public DebuggableTemporaryFolder folder = new DebuggableTemporaryFolder();


  @Test
  public void whenCaptureRunOnVoidReturnThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {

    String src_file =
        "infer/tests/codetoanalyze/objc/frontend/" +
            "returnstmt/void_return.m";

    String dotty_file =
        "infer/tests/codetoanalyze/objc/frontend/" +
            "returnstmt/void_return.dot";

    ImmutableList<String> inferCmd =
        InferRunner.createObjCInferCommandFrontend(folder, src_file);
    File newDotFile = InferRunner.runInferFrontend(inferCmd);
    assertThat(
        "In the capture of " + src_file +
            " the dotty files should be the same.",
        newDotFile, dotFileEqualTo(dotty_file));
  }

}
