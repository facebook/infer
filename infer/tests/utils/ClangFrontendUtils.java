/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package utils;

import static org.hamcrest.MatcherAssert.assertThat;
import static utils.matchers.DotFilesEqual.dotFileEqualTo;

import com.google.common.collect.ImmutableList;

import java.io.File;
import java.io.IOException;

import utils.DebuggableTemporaryFolder;
import utils.InferException;
import utils.InferRunner;

public class ClangFrontendUtils {
  public static void createAndCompareCppDotFiles(DebuggableTemporaryFolder folder, String pathToSrcFile)
      throws InterruptedException, IOException, InferException {

    String test_src = pathToSrcFile;
    String test_dotty = pathToSrcFile + ".dot";
    ImmutableList<String> inferCmd =
        InferRunner.createCPPInferCommandFrontend(
            folder,
            test_src);
    File newDotFile = InferRunner.runInferFrontend(inferCmd);
    assertThat(
        "In the capture of " + test_src +
            " the dotty files should be the same.",
        newDotFile, dotFileEqualTo(test_dotty));
  }

  public static void createAndCompareCDotFiles(DebuggableTemporaryFolder folder, String pathToSrcFile)
      throws InterruptedException, IOException, InferException {

    String test_src = pathToSrcFile;
    String test_dotty = pathToSrcFile + ".dot";
    ImmutableList<String> inferCmd =
        InferRunner.createCInferCommandFrontend(
            folder,
            test_src);
    File newDotFile = InferRunner.runInferFrontend(inferCmd);
    assertThat(
        "In the capture of " + test_src +
            " the dotty files should be the same.",
        newDotFile, dotFileEqualTo(test_dotty));
  }
}
