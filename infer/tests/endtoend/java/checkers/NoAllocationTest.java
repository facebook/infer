/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */


package endtoend.java.checkers;

import org.junit.BeforeClass;
import org.junit.Test;
import utils.InferException;
import utils.InferResults;

import java.io.IOException;

import static org.hamcrest.MatcherAssert.assertThat;
import static utils.matchers.ResultContainsExactly.containsExactly;

public class NoAllocationTest {

  public static final String SOURCE_FILE =
    "infer/tests/codetoanalyze/java/checkers/NoAllocationExample.java";

  public static final String ALLOCATES_MEMORY =
    "CHECKERS_ALLOCATES_MEMORY";

  private static InferResults inferResults;

  @BeforeClass
  public static void loadResults() throws InterruptedException, IOException {
    inferResults = InferResults.loadCheckersResults(NoAllocationTest.class, SOURCE_FILE);
  }

  @Test
  public void matchErrors()
    throws IOException, InterruptedException, InferException {
    String[] methods = {
      "directlyAllocatingMethod",
      "indirectlyAllocatingMethod",
    };
    assertThat("Results should contain " + ALLOCATES_MEMORY,
               inferResults,
               containsExactly(ALLOCATES_MEMORY,
                               SOURCE_FILE,
                               methods));
  }

}
