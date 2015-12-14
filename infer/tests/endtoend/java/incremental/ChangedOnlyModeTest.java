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

public class ChangedOnlyModeTest {

  public static final String SOURCE_DIR =
      "/infer/tests/codetoanalyze/java/incremental/changed_only_mode/";

  private static InferStats inferStats;

  @BeforeClass
  public static void loadResults() throws InterruptedException, IOException {
    inferStats = InferStats.loadInferStats(ChangedOnlyModeTest.class, SOURCE_DIR);
  }

  @Test
  public void onlyChangedFileReanalyzedInChangedOnlyMode()
      throws IOException, InterruptedException, InferException {
    assertThat(
        "After changing the child file, parent should not be re-analyzed in changed-only mode",
        inferStats,
        numberOfFilesAnalyzed(1));
  }

  @Test
  public void onlyChangedProcsReanalyzedInIncrementalMode()
      throws IOException, InterruptedException, InferException {
    assertThat(
        "After changing a procedure, its callers should not be re-analyzed in changed-only mode",
        inferStats,
        numberOfProceduresAnalyzed(1));
  }

}
