/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package endtoend.cpp;

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

public class SharedPtrConstructorsTest {

  public static final String FILE =
      "infer/tests/codetoanalyze/cpp/errors/smart_ptr/shared_ptr_constructors.cpp";

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
  public void whenInferRunsNullDerefFunctionsErrorIsFound()
      throws InterruptedException, IOException, InferException {
    String[] procedures = {
        "get_from_base1_nullptr_deref",
        "get_from_base2_nullptr_deref",
        "get_from_derived1_nullptr_deref",
        "get_from_derived2_nullptr_deref",
        "get_from_derived3_nullptr_deref",
        "get_from_base1_null_f1_deref",
        "get_from_base2_null_f1_deref",
        "get_from_derived1_null_f1_deref",
        "get_from_derived2_null_f1_deref",
        "get_from_derived3_null_f1_deref",
    };
    InferResults inferResults = InferRunner.runInferCPP(inferCmd);
    assertThat(
        "Results should contain divide by 0 error",
        inferResults,
        containsExactly(
            NULL_DEREFERENCE,
            FILE,
            procedures
        )
    );
  }
}
