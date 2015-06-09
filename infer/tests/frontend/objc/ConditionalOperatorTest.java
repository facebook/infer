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

public class ConditionalOperatorTest {

  @Rule
  public DebuggableTemporaryFolder folder = new DebuggableTemporaryFolder();


  @Test
  public void whenCaptureRunCommaThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    String cond_src =
        "infer/tests/codetoanalyze/objc/frontend/conditional_operation/ConditionalOperation.m";

    String cond_dotty =
        "infer/tests/codetoanalyze/objc/frontend/conditional_operation/ConditionalOperation.dot";

    ImmutableList<String> inferCmd =
        InferRunner.createObjCInferCommandFrontend(
            folder,
            cond_src);
    File newDotFile = InferRunner.runInferFrontend(inferCmd);
    assertThat(
        "In the capture of " + cond_src + " the dotty files should be the same.",
        newDotFile, dotFileEqualTo(cond_dotty));
  }
}
