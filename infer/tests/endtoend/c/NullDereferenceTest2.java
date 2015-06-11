/*
 * Copyright (c) 2013- Facebook.
 * All rights reserved.
 */

package endtoend.c;

import static org.hamcrest.MatcherAssert.assertThat;
import static utils.matchers.ResultContainsErrorInMethod.contains;


import org.junit.BeforeClass;
import org.junit.Test;

import java.io.IOException;

import utils.InferException;
import utils.InferResults;

public class NullDereferenceTest2 {

  public static final String SOURCE_FILE =
      "null_dereference/get.c";


  public static final String NULL_DEREFERENCE = "NULL_DEREFERENCE";

  private static InferResults inferResults;

  @BeforeClass
  public static void runInfer() throws InterruptedException, IOException {
    inferResults = InferResults.loadCInferResults(
        NullDereferenceTest2.class,
        SOURCE_FILE);
  }

  /*
  @Test
  public void nullDereferenceTest2() throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should contain null pointer dereference error",
        inferResults,
        contains(
            NULL_DEREFERENCE,
            SOURCE_FILE,
            "crashgetc"
        )
    );
  }

  @Test
  public void nullDereferenceTest2_fgetc() throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should contain null pointer dereference error",
        inferResults,
        contains(
            NULL_DEREFERENCE,
            SOURCE_FILE,
            "crashfgetc"
        )
    );
  }
    */
}
