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

public class AngelismTest {

  public static final String SOURCE_FILE =
      "null_dereference/angelism.c";

  public static final String NULL_DEREFERENCE = "NULL_DEREFERENCE";
  public static final String DIVIDE_BY_ZERO = "DIVIDE_BY_ZERO";

  private static InferResults inferResults;

  @BeforeClass
  public static void runInfer() throws InterruptedException, IOException {
    inferResults = InferResults.loadCInferResults(
        AngelismTest.class,
        SOURCE_FILE);
  }

  @Test
  public void angelismNPETest() throws InterruptedException, IOException, InferException {
    String[] procedures = {
        "bake",
        "call_by_ref_actual_already_in_footprint_bad",
        "struct_value_by_ref_ptr_write",
        "struct_value_by_ref_callee_write_no_skip",
        "struct_value_by_ref_callee_write_skip",
        "struct_value_skip_null_deref",
        "struct_value_from_pointer_skip_bad"
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

}
