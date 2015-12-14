/*
 * Copyright (c) 2015 - present Facebook, Inc.
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

public class NSNumberTest {

  public static final String NSNUMBER_FILE =
      "infer/tests/codetoanalyze/objc/errors/bad_ptr_comparisons/nsnumber.m";

  private static ImmutableList<String> inferCmd;

  public static final String BAD_POINTER_COMPARISON = "BAD_POINTER_COMPARISON";

  @ClassRule
  public static DebuggableTemporaryFolder folder = new DebuggableTemporaryFolder();

  @BeforeClass
  public static void runInfer() throws InterruptedException, IOException {
    inferCmd = InferRunner.createObjCInferCommand(
        folder,
        NSNUMBER_FILE);
  }

  @Test
  public void badNSNumberPointerComparisonShouldBeFound()
      throws InterruptedException, IOException, InferException {
    InferResults inferResults = InferRunner.runInferObjC(inferCmd);
    String[] methods = {
      "bad1",
      "bad2"
    };
    assertThat(
        "Results should contain " + BAD_POINTER_COMPARISON,
        inferResults,
        containsExactly(
            BAD_POINTER_COMPARISON,
            NSNUMBER_FILE,
            methods
        )
    );
  }

}
