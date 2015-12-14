/*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package endtoend.java.harness;


import static org.hamcrest.MatcherAssert.assertThat;
import static utils.matchers.ResultContainsErrorNoFilename.contains;

import com.google.common.collect.ImmutableList;

import org.junit.BeforeClass;
import org.junit.ClassRule;
import org.junit.Test;

import java.io.IOException;

import utils.DebuggableTemporaryFolder;
import utils.InferException;
import utils.InferResults;
import utils.InferRunner;

public class FindViewByIdTest {

  public static final String FindViewByIdActivity =
      "infer/tests/codetoanalyze/java/harness/FindViewByIdActivity.java";

  public static final String MyView =
      "infer/tests/codetoanalyze/java/harness/MyView.java";

  public static final String NULL_DEREFERENCE = "NULL_DEREFERENCE";

  @ClassRule
  public static DebuggableTemporaryFolder folder = new DebuggableTemporaryFolder();

  private static ImmutableList<String> inferCmd;


  @BeforeClass
  public static void runInfer() throws IOException {
    inferCmd = InferRunner.createJavaInferAngelicHarnessCommand(
        folder,
        ImmutableList.of(FindViewByIdActivity, MyView));
  }


  @Test
  public void harnessRevealsNpe()
      throws InterruptedException, IOException, InferException {
    InferResults inferResults = InferRunner.runInferJava(inferCmd);
    assertThat(
        "Results should contain NPE",
        inferResults,
        contains(
            NULL_DEREFERENCE,
            "java.harness.FindViewByIdActivity.InferGeneratedHarness"
        )
    );
  }

}
