/*
 * Copyright (c) 2013- Facebook.
 * All rights reserved.
 */

package frontend.c;

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

public class EnumerationTest {

  @Rule
  public DebuggableTemporaryFolder folder = new DebuggableTemporaryFolder();


  @Test
  public void whenCaptureRunEnumThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    String enum_expr =
        "infer/tests/codetoanalyze/c/frontend/enumeration/enum.c";

    String enum_dotty =
        "infer/tests/codetoanalyze/c/frontend/enumeration/enum.dot";

    ImmutableList<String> inferCmd =
        InferRunner.createCInferCommandFrontend(folder, enum_expr);
    File newDotFile = InferRunner.runInferFrontend(inferCmd);
    assertThat(
        "In the capture of " + enum_expr +
            " the dotty files should be the same.",
        newDotFile, dotFileEqualTo(enum_dotty));
  }


}
