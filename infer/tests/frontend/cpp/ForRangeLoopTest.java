/*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package frontend.cpp;

import org.junit.Rule;
import org.junit.Test;
import utils.ClangFrontendUtils;
import utils.DebuggableTemporaryFolder;
import utils.InferException;

import java.io.IOException;

public class ForRangeLoopTest {

  @Rule
  public DebuggableTemporaryFolder folder = new DebuggableTemporaryFolder();

  @Test
  public void whenCaptureRunOnForEach1ThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    String src =
        "infer/tests/codetoanalyze/cpp/frontend/loops/foreach1.cpp";
    ClangFrontendUtils.createAndCompareCppDotFiles(folder, src);
  }
}
