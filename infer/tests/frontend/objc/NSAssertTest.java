/*
 * Copyright (c) 2014 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package frontend.objc;

import static org.hamcrest.MatcherAssert.assertThat;
import static utils.matchers.DotFilesEqual.dotFileEqualTo;

import com.google.common.collect.ImmutableList;

import org.junit.Assume;
import org.junit.Rule;
import org.junit.Test;

import java.io.File;
import java.io.IOException;

import utils.DebuggableTemporaryFolder;
import utils.InferException;
import utils.InferRunner;


public class NSAssertTest {

  @Rule
  public DebuggableTemporaryFolder folder = new DebuggableTemporaryFolder();


  @Test
  public void whenCaptureRunOnPropertyThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {

    String src = "infer/tests/codetoanalyze/objc/frontend/assertions/NSAssert_example.m";

    String dotty = "infer/tests/codetoanalyze/objc/frontend/assertions/NSAssert_example.dot";

    ImmutableList<String> inferCmd =
        InferRunner.createObjCInferCommandFrontendArc(folder, src);
    File newDotFile = InferRunner.runInferFrontend(inferCmd);
    assertThat(
        "In the capture of " + src + " the dotty files should be the same.",
        newDotFile, dotFileEqualTo(dotty));
  }
}
