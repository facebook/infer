/*
 * Copyright (c) 2013- Facebook.
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

public class NullDereferenceTest2 {

  public static final String SOURCE_FILE =
      "null_dereference/getc.c";


  public static final String NULL_DEREFERENCE = "NULL_DEREFERENCE";

  private static InferResults inferResults;

  @BeforeClass
  public static void runInfer() throws InterruptedException, IOException {
    inferResults = InferResults.loadCInferResults(
        NullDereferenceTest2.class,
        SOURCE_FILE);
  }

  @Test
  public void nullDereferenceTest() throws InterruptedException, IOException, InferException {
    String[] procedures = {
        "crash_getc",
        "crash_fgetc",
    };
    System.out.println(inferResults.toString());
    assertThat(
        "Results should contain null pointer dereference error",
        inferResults,
        containsExactly(
            NULL_DEREFERENCE,
            SOURCE_FILE,
            procedures
        )
    );
  }
}
