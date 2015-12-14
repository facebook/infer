/*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package endtoend.objc;

import static org.hamcrest.MatcherAssert.assertThat;
import static utils.matchers.ResultContainsErrorInMethod.contains;
import static utils.matchers.ResultContainsOnlyTheseErrors.containsOnly;

import com.google.common.collect.ImmutableList;

import org.junit.BeforeClass;
import org.junit.ClassRule;
import org.junit.Test;

import java.io.IOException;

import utils.DebuggableTemporaryFolder;
import utils.InferException;
import utils.InferResults;
import utils.InferRunner;

public class NullDerefObjCClassTest {

  public static final String FRACTION_FILE =
      "infer/tests/codetoanalyze/objc/errors/npe/Fraction.m";

  private static ImmutableList<String> inferCmdFraction;

  public static final String NULL_DEREFERENCE = "NULL_DEREFERENCE";

  @ClassRule
  public static DebuggableTemporaryFolder folderFraction =
      new DebuggableTemporaryFolder();

  @BeforeClass
  public static void runInfer() throws InterruptedException, IOException {
    inferCmdFraction = InferRunner.createObjCInferCommandWithMLBuckets(
        folderFraction,
        FRACTION_FILE,
        "cf",
        false);

  }

  @Test
  public void whenInferRunsOnNull_deref_objc_classThenNpeIsFound()
      throws InterruptedException, IOException, InferException {
    InferResults inferResults = InferRunner.runInferC(inferCmdFraction);
    assertThat(
        "Results should contain null pointer dereference error",
        inferResults,
        contains(
            NULL_DEREFERENCE,
            FRACTION_FILE,
            "test_virtual_call"
        )
    );
  }

  @Test
  public void whenInferRunsOnNpeThenOnlyTheExpectedErrorsAreFound()
      throws InterruptedException, IOException, InferException {
    InferResults inferResults = InferRunner.runInferC(inferCmdFraction);
    String[] expectedProcedures = {
        "test_virtual_call"
    };
    assertThat(
        "No unexpected errors should be found", inferResults,
        containsOnly(
            NULL_DEREFERENCE,
            FRACTION_FILE,
            expectedProcedures));
  }

}
