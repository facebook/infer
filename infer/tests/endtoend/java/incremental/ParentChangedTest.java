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

public class ParentChangedTest {

  public static final String SOURCE_DIR =
      "/infer/tests/codetoanalyze/java/incremental/parent_changed/";

  private static InferStats inferStats;

  @BeforeClass
  public static void loadResults() throws InterruptedException, IOException {
    inferStats = InferStats.loadInferStats(ParentChangedTest.class, SOURCE_DIR);
  }

  @Test
  public void unchangedFileNotReanalyzedInIncrementalMode()
      throws IOException, InterruptedException, InferException {
    assertThat(
        "After changing only the parent file, the child file should not be re-analyzed",
        inferStats,
        numberOfFilesAnalyzed(1));
    assertThat(
        "When adding a new procedure, the old ones should not be re-analyzed",
        inferStats,
        numberOfProceduresAnalyzed(1));
  }

  @Test
  public void unchangedProcedureNotReanalyzedInIncrementalMode()
      throws IOException, InterruptedException, InferException {
    assertThat(
        "When adding a new procedure, the old ones should not be re-analyzed",
        inferStats,
        numberOfProceduresAnalyzed(1));
  }

}
