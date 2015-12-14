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

public class FileUnchangedTest {

  public static final String SOURCE_DIR =
      "/infer/tests/codetoanalyze/java/incremental/file_unchanged/";

  private static InferStats inferStats;

  @BeforeClass
  public static void loadResults() throws InterruptedException, IOException {
    inferStats = InferStats.loadInferStats(FileUnchangedTest.class, SOURCE_DIR);
  }

  @Test
  public void unchangedFileNotReanalyzedInIncrementalMode()
      throws IOException, InterruptedException, InferException {
    assertThat(
        "Unchanged file should not be re-analyzed in incremental mode",
        inferStats,
        numberOfFilesAnalyzed(0));
  }

  @Test
  public void unchangedProcdureNotReanalyzedInIncrementalMode()
      throws IOException, InterruptedException, InferException {
    assertThat(
        "Unchanged procedure should not be re-analyzed in incremental mode",
        inferStats,
        numberOfProceduresAnalyzed(0));
  }

}
