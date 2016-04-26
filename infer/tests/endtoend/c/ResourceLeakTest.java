/*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package endtoend.c;

import static org.hamcrest.MatcherAssert.assertThat;
import static utils.matchers.ResultContainsExactly.containsExactly;

import org.junit.BeforeClass;
import org.junit.Test;

import java.io.IOException;

import utils.InferException;
import utils.InferResults;

public class ResourceLeakTest {

  public static final String SOURCE_FILE =
      "resource_leaks/leak.c";

  public static final String RESOURCE_LEAK = "RESOURCE_LEAK";

  private static InferResults inferResults;

  @BeforeClass
  public static void runInfer() throws InterruptedException, IOException {
    inferResults = InferResults.loadCInferResults(ResourceLeakTest.class, SOURCE_FILE);
  }

  @Test
  public void mathErrors()
      throws InterruptedException, IOException, InferException {
    String[] functions = {
        "fileNotClosed",
        "socketNotClosed"
    };
    assertThat(
        "Results should contain " + RESOURCE_LEAK,
        inferResults,
        containsExactly(
            RESOURCE_LEAK,
            SOURCE_FILE,
            functions
        )
    );
  }

}
