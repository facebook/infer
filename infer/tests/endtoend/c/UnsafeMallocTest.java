/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package endtoend.c;

import com.google.common.collect.ImmutableList;

import static org.hamcrest.MatcherAssert.assertThat;
import static utils.matchers.ResultContainsNoErrorInMethod.doesNotContain;

import org.junit.BeforeClass;
import org.junit.ClassRule;
import org.junit.Test;

import java.io.IOException;

import utils.DebuggableTemporaryFolder;
import utils.InferException;
import utils.InferResults;
import utils.InferRunner;

public class UnsafeMallocTest {

  public static final String SOURCE_FILE =
      "infer/tests/codetoanalyze/c/errors/null_dereference/malloc_no_null_check.c";

  public static final String NULL_DEREFERENCE = "NULL_DEREFERENCE";
  private static ImmutableList<String> inferCommand;

  @ClassRule
  public static DebuggableTemporaryFolder folder = new DebuggableTemporaryFolder();

  @BeforeClass
  public static void runInfer() throws InterruptedException, IOException {
    inferCommand = InferRunner.createCInferCommand(
      folder,
      SOURCE_FILE,
      ImmutableList.<String>of("--unsafe-malloc"));
  }

  @Test
  public void unsafeMallocTest() throws InterruptedException, IOException, InferException {
    InferResults inferResults = InferRunner.runInferC(inferCommand);

    assertThat(
        "Results should not contain null pointer dereference error",
        inferResults,
        doesNotContain(
            NULL_DEREFERENCE,
            SOURCE_FILE,
            "test_malloc"
        )
    );
  }

}
