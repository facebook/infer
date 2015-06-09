/*
 * Copyright (c) 2015- Facebook.
 * All rights reserved.
 */

package endtoend.c;

import static org.hamcrest.MatcherAssert.assertThat;
import static utils.matchers.ResultContainsExactly.containsExactly;

import org.junit.BeforeClass;
import org.junit.Test;

import java.io.IOException;

import utils.InferException;
import utils.InferResults;

public class MemoryLeakTest {

  public static final String SOURCE_FILE = "memory_leaks/test.c";

  public static final String MEMORY_LEAK = "MEMORY_LEAK";

  private static InferResults inferResults;

  @BeforeClass
  public static void runInfer() throws InterruptedException, IOException {
    inferResults = InferResults.loadCInferResults(
        MemoryLeakTest.class,
        SOURCE_FILE);
  }

  @Test
  public void memoryLeakTest() throws InterruptedException, IOException, InferException {
    String[] procedures = {
        "simple_leak",
        "uses_allocator",
        "common_realloc_leak",
    };
    assertThat(
        "Results should contain the expected memory leak errors",
        inferResults,
        containsExactly(
            MEMORY_LEAK,
            SOURCE_FILE,
            procedures
        )
    );
  }



}
