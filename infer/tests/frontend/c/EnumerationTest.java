/*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package frontend.c;

import org.junit.Rule;
import org.junit.Test;

import java.io.IOException;

import utils.DebuggableTemporaryFolder;
import utils.InferException;
import utils.ClangFrontendUtils;

public class EnumerationTest {

  @Rule
  public DebuggableTemporaryFolder folder = new DebuggableTemporaryFolder();


  @Test
  public void whenCaptureRunEnumThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    String src = "infer/tests/codetoanalyze/c/frontend/enumeration/enum.c";
    ClangFrontendUtils.createAndCompareCDotFiles(folder, src);
  }

  @Test
  public void whenCaptureRunOnOtherEnumThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    String src = "infer/tests/codetoanalyze/c/frontend/enumeration/other_enum.c";
    ClangFrontendUtils.createAndCompareCDotFiles(folder, src);
  }

  @Test
  public void whenCaptureRunOnEnumBitmaskThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    String src = "infer/tests/codetoanalyze/c/frontend/enumeration/enum_bitmask.c";
    ClangFrontendUtils.createAndCompareCDotFiles(folder, src);
  }

}
