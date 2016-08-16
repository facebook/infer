/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package frontend.objcpp;

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

public class GlobalConstTest {

  @Rule
  public DebuggableTemporaryFolder folder = new DebuggableTemporaryFolder();

  @Test
  public void whenCaptureRunSwitchStmtThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    String switch_src =
        "infer/tests/codetoanalyze/objcpp/frontend/global_const/global_const.mm";

    String switch_dotty =
        "infer/tests/codetoanalyze/objcpp/frontend/global_const/global_const.mm.dot";

    ImmutableList<String> inferCmd =
        InferRunner.createObjCPPInferCommand(folder, switch_src);
    File newDotFile = InferRunner.runInferFrontend(inferCmd);
    assertThat(
        "In the capture of " + switch_src +
            " the dotty files should be the same.",
        newDotFile, dotFileEqualTo(switch_dotty));
  }
}
