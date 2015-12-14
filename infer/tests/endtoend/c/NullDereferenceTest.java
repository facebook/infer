/*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package endtoend.c;

import static org.hamcrest.MatcherAssert.assertThat;
import static utils.matchers.ResultContainsLineNumbers.containsLines;
import static utils.matchers.ResultContainsExactly.containsExactly;

import org.junit.BeforeClass;
import org.junit.Test;

import java.io.IOException;

import utils.InferException;
import utils.InferResults;

public class NullDereferenceTest {

  public static final String SOURCE_FILE =
      "null_dereference/null_pointer_dereference.c";


  public static final String NULL_DEREFERENCE = "NULL_DEREFERENCE";

  private static InferResults inferResults;

  @BeforeClass
  public static void runInfer() throws InterruptedException, IOException {
    inferResults = InferResults.loadCInferResults(
        NullDereferenceTest.class,
        SOURCE_FILE);
  }

  @Test
  public void nullDereferenceTest() throws InterruptedException, IOException, InferException {
    String[] procedures = {
        "basic_null_dereference",
        "simple_null_pointer",
        "null_pointer_interproc",
        "null_pointer_with_function_pointer",
        "no_check_for_null_after_malloc",
        "no_check_for_null_after_realloc",
        "potentially_null_pointer_passed_as_argument",
        "null_passed_as_argument",
        "function_call_can_return_null_pointer",
    };
    assertThat(
        "Results should contain null pointer dereference error",
        inferResults,
        containsExactly(
            NULL_DEREFERENCE,
            SOURCE_FILE,
            procedures
        )
    );
  }

  @Test
  public void whenInferRunsOnSimpleNpe_interprocThenCorrectLineIsReported()
      throws InterruptedException, IOException, InferException {
    int[] lines = {20, 34};
    assertThat(
        "Results should contain null pointer dereference error",
        inferResults,
        containsLines(lines));
  }

}
