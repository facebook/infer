/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package endtoend.java.incremental;

import static org.hamcrest.MatcherAssert.assertThat;
import static utils.matchers.NumberOfFilesAnalyzed.numberOfFilesAnalyzed;
import static utils.matchers.NumberOfProceduresAnalyzed.numberOfProceduresAnalyzed;

import org.junit.BeforeClass;
import org.junit.Test;

import java.io.IOException;
import java.nio.file.Paths;
import java.nio.file.Path;
import java.io.*;

import utils.InferException;
import utils.InferStats;

public class ChildChangedTest {

  public static final String SOURCE_DIR =
      "/infer/tests/codetoanalyze/java/incremental/child_changed/";

  private static InferStats inferStats;

  @BeforeClass
  public static void loadResults() throws InterruptedException, IOException {
    inferStats = InferStats.loadInferStats(ChildChangedTest.class, SOURCE_DIR);
  }

  @Test
  public void changedFilesReanalyzedInIncrementalMode()
      throws IOException, InterruptedException, InferException {
    assertThat(
        "After changing the child file, parent and grandparent should be re-analyzed",
        inferStats,
        numberOfFilesAnalyzed(3));
  }

  @Test
  public void onlyChangedProcsReanalyzedInIncrementalMode()
      throws IOException, InterruptedException, InferException {
    assertThat(
        "After changing a procedure, only the proc and its callers should be re-analyzed",
        inferStats,
        numberOfProceduresAnalyzed(3));
  }

}
