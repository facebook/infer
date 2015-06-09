/*
 * Copyright (c) 2013- Facebook.
 * All rights reserved.
 */

package frontend.cpp;

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

public class NamespaceTest {

  @Rule
  public DebuggableTemporaryFolder folder = new DebuggableTemporaryFolder();

  @Test
  public void whenCaptureRunCommaThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    String switch_src =
        "infer/tests/codetoanalyze/cpp/frontend/namespace/namespace.cpp";

    String switch_dotty =
        "infer/tests/codetoanalyze/cpp/frontend/namespace/namespace.dot";

    ImmutableList<String> inferCmd =
        InferRunner.createObjCPPInferCommandFrontend(
            folder,
            switch_src);
    File newDotFile = InferRunner.runInferFrontend(inferCmd);
    assertThat(
        "In the capture of " + switch_src +
            " the dotty files should be the same.",
        newDotFile, dotFileEqualTo(switch_dotty));
  }
}
