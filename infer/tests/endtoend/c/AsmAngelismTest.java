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
import static utils.matchers.ResultContainsExactly.containsExactly;

import org.junit.BeforeClass;
import org.junit.Test;

import java.io.IOException;

import utils.InferException;
import utils.InferResults;

public class AsmAngelismTest {

  public static final String SOURCE_FILE = "null_dereference/asm_angelism.c";

  public static final String NULL_DEREFERENCE = "NULL_DEREFERENCE";

  private static InferResults inferResults;

  @BeforeClass
  public static void runInfer() throws InterruptedException, IOException {
    inferResults = InferResults.loadCInferResults(
        AsmAngelismTest.class,
        SOURCE_FILE);
  }

  @Test
  public void angelismNPETest() throws InterruptedException, IOException, InferException {
    String[] procedures = {};
    assertThat(
        "Results should not contain null pointer dereference error",
        inferResults,
        containsExactly(
            NULL_DEREFERENCE,
            SOURCE_FILE,
            procedures
        )
    );
  }

}
