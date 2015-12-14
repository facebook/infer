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
        "crash_ungetc",
        "crash_fputs",
        "crash_fputc",
        "crash_putc",
        "crash_fseeks",
        "crash_ftell",
        "crash_fgets",
        "crash_rewind",
        "crash_fileno",
        "crash_clearerr",
        "crash_ferror",
        "crash_feof",
        "crash_fprintf",
        "crash_vfprintf",
        "crash_fgetpos",
        "crash_fsetpos",
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
