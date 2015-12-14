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

public class NullableTest {

  public static final String NPE_FILE = "infer/tests/codetoanalyze/objc/errors/npe/nullable.m";

  private static ImmutableList<String> inferCmdNPD;

  public static final String NULL_DEREFERENCE = "NULL_DEREFERENCE";

  @ClassRule
  public static DebuggableTemporaryFolder folderNPD = new DebuggableTemporaryFolder();

  @BeforeClass
  public static void runInfer() throws InterruptedException, IOException {
    inferCmdNPD = InferRunner.createiOSInferCommandWithMLBuckets(
        folderNPD,
        NPE_FILE,
        "cf",
        true);
  }

  @Test
  public void nullDereferenceTest() throws InterruptedException, IOException, InferException {
    InferResults inferResults = InferRunner.runInferC(inferCmdNPD);
    String[] procedures = {
      "derefNullableParamDirect",
      "derefNullableParamIndirect"
    };
    assertThat(
        "Results should contain null pointer dereference error",
        inferResults,
        containsExactly(
            NULL_DEREFERENCE,
            NPE_FILE,
            procedures
        )
    );
  }

}
