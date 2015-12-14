/*
 * Copyright (c) 2013 - present Facebook, Inc.
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

import org.junit.Rule;
import org.junit.Test;

import java.io.File;
import java.io.IOException;

import utils.DebuggableTemporaryFolder;
import utils.InferException;
import utils.InferRunner;

public class ProtocolTest {

  @Rule
  public DebuggableTemporaryFolder folder = new DebuggableTemporaryFolder();


  @Test
  public void whenCaptureRunOnProtocolThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    String protocol_expr =
        "infer/tests/codetoanalyze/" +
            "objc/frontend/protocol/protocol.m";

    String protocol_dotty =
        "infer/tests/codetoanalyze/" +
            "objc/frontend/protocol/protocol.dot";

    ImmutableList<String> inferCmd =
        InferRunner.createObjCInferCommandFrontend(
            folder,
            protocol_expr);
    File newDotFile = InferRunner.runInferFrontend(inferCmd);
    assertThat(
        "In the capture of " + protocol_expr +
            " the dotty files should be the same. " +
            "In each procedure the translation of the boxing " +
            "syntactic sugar is the same " +
            "as the tranlation of the underlying method call.",
        newDotFile, dotFileEqualTo(protocol_dotty));
  }

  @Test
  public void whenCaptureRunOnBicycleThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    String protocol_expr =
        "infer/tests/codetoanalyze/" +
            "objc/errors/protocol_procdesc/main.c";

    String protocol_dotty =
        "infer/tests/codetoanalyze/" +
            "objc/errors/protocol_procdesc/main.dot";

    ImmutableList<String> inferCmd =
        InferRunner.createObjCInferCommandFrontend(
            folder,
            protocol_expr);
    File newDotFile = InferRunner.runInferFrontend(inferCmd);
    assertThat(
        "In the capture of " + protocol_expr +
            " the dotty files should be the same.",
        newDotFile, dotFileEqualTo(protocol_dotty));
  }

}
