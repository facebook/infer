/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package endtoend.cpp;

import static org.hamcrest.MatcherAssert.assertThat;
import static utils.matchers.ResultContainsErrorInMethod.contains;
import static utils.matchers.ResultContainsExactly.containsExactly;
import static utils.matchers.ResultContainsNoErrorInMethod.doesNotContain;

import com.google.common.collect.ImmutableList;

import org.junit.BeforeClass;
import org.junit.ClassRule;
import org.junit.Test;

import java.io.IOException;

import utils.DebuggableTemporaryFolder;
import utils.InferException;
import utils.InferResults;
import utils.InferRunner;

public class NPEWhenMethodCallTest {

  public static final String FILE =
      "infer/tests/codetoanalyze/cpp/errors/npe/method_call.cpp";

  private static ImmutableList<String> inferCmd;

  public static final String NULL_DEREFERENCE = "NULL_DEREFERENCE";

  @ClassRule
  public static DebuggableTemporaryFolder folder =
      new DebuggableTemporaryFolder();

  @BeforeClass
  public static void runInfer() throws InterruptedException, IOException {
    inferCmd = InferRunner.createCPPInferCommand(folder, FILE);
  }

  @Test
  public void whenInferRunsOnMethodCallThenNoNPEFound()
      throws InterruptedException, IOException, InferException {
    InferResults inferResults = InferRunner.runInferCPP(inferCmd);
    String[] procedures = {"npe_call", "npe_call_after_call", "npe_call_with_forward_declaration"};
    assertThat(
        "Results should contain " + NULL_DEREFERENCE,
        inferResults,
        containsExactly(
            NULL_DEREFERENCE,
            FILE,
            procedures
        )
    );
  }
}
