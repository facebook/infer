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

public class MallocTest {

  @Rule
  public DebuggableTemporaryFolder folder = new DebuggableTemporaryFolder();


  @Test
  public void whenCaptureRunLateDefinedStaticVarThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    String src_file =
        "infer/tests/codetoanalyze/objc/errors/npe/npe_malloc.m";

    String dot_file =
        "infer/tests/codetoanalyze/objc/errors/npe/npe_malloc.dot";

    ImmutableList<String> inferCmd =
        InferRunner.createObjCInferCommandFrontend(folder, src_file);
    File newDotFile = InferRunner.runInferFrontend(inferCmd);
    assertThat(
        "In the capture of " + src_file +
            " the dotty files should be the same.",
        newDotFile, dotFileEqualTo(dot_file));
  }
}
