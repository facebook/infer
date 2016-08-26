/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package endtoend.cpp.infer;

import static org.hamcrest.MatcherAssert.assertThat;
import static utils.matchers.ResultContainsExactly.containsExactly;

import com.google.common.collect.ImmutableList;

import org.junit.BeforeClass;
import org.junit.ClassRule;
import org.junit.Test;

import java.io.IOException;

import utils.DebuggableTemporaryFolder;
import utils.InferException;
import utils.InferResults;
import utils.InferRunner;

public class DeprecatedHackTest {

  public static final String SOURCE_FILE =
      "infer/tests/codetoanalyze/cpp/frontend/attributes/deprecated_hack.cpp";

  public static final String NULL_DEREFERENCE = "NULL_DEREFERENCE";
  private static ImmutableList<String> inferCmd;

  @ClassRule
  public static DebuggableTemporaryFolder folder =
      new DebuggableTemporaryFolder();

  @BeforeClass
  public static void runInfer() throws InterruptedException, IOException {
    inferCmd = InferRunner.createCPPInferCommand(folder, SOURCE_FILE);
  }

  @Test
  public void whenInferRunsOnNullDerefThenNullDereferenceIsFound()
      throws InterruptedException, IOException, InferException {
    InferResults inferResults = InferRunner.runInferC(inferCmd);
    String[] procedures = {
      "derefFirstArg_null_deref",
      "derefFirstArg2_null_deref",
      "derefFirstArg3_null_deref",
      "getPtr_null_deref1",
      "getPtr_null_deref2",
      "operator_star_null_deref1",
      "operator_star_null_deref2",
      "getRef_null_deref1",
      "getRef_null_deref2",
    };
    assertThat(
        "Results should contain divide by zero error",
        inferResults,
        containsExactly(
            NULL_DEREFERENCE,
            SOURCE_FILE,
            procedures
        )
    );
  }


}
