/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package endtoend.c.infer;

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

public class ArrayOfStructsAbductionTest {

  public static final String source_file =
      "infer/tests/codetoanalyze/c/errors/initialization/abduce_structured_types.c";

  private static ImmutableList<String> inferCmd;

  public static final String BAD_FOOTPRINT = "Bad_footprint";

  @ClassRule
  public static DebuggableTemporaryFolder folder = new DebuggableTemporaryFolder();

  @BeforeClass
  public static void runInfer() throws InterruptedException, IOException {
    ImmutableList<String> extraOptions =
      new ImmutableList.Builder<String>()
      .add("--developer-mode")
      .build();
    inferCmd = InferRunner.createCInferCommand(folder, source_file, extraOptions);
  }

  @Test
  public void whenInferRunsOnAssertExampleThenNPENotFound()
      throws InterruptedException, IOException, InferException {
    InferResults inferResults = InferRunner.runInferC(inferCmd);
    String[] no_procedures = {};

    assertThat("Results should not contain a bad footprint error",
               inferResults,
               containsExactly(BAD_FOOTPRINT,
                               source_file,
                               no_procedures));
  }

}
