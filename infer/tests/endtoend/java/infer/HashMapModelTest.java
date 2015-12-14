/*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package endtoend.java.infer;

import static org.hamcrest.MatcherAssert.assertThat;
import static utils.matchers.ResultContainsErrorInMethod.contains;
import static utils.matchers.ResultContainsNoErrorInMethod.doesNotContain;

import org.junit.BeforeClass;
import org.junit.Test;

import java.io.IOException;

import utils.InferException;
import utils.InferResults;

public class HashMapModelTest {

  public static final String HashMapModelTest =
      "infer/tests/codetoanalyze/java/infer/HashMapExample.java";

  public static final String NULL_DEREFERENCE = "NULL_DEREFERENCE";

  private static InferResults inferResults;

  @BeforeClass
  public static void loadResults() throws InterruptedException, IOException {
    inferResults = InferResults.loadInferResults(
      HashMapModelTest.class,
      HashMapModelTest);
  }

  @Test
  public void whenInferRunsOnPutIntegerTwiceThenGetTwiceThenNPEIsNotFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
      "Results should not contain null pointer exception error",
      inferResults,
      doesNotContain(
        NULL_DEREFERENCE,
        HashMapModelTest,
        "putIntegerTwiceThenGetTwice")
    );
  }

  @Test
  public void whenInferRunsOnContainsIntegerTwiceThenGetTwiceThenNPEIsNotFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
      "Results should contain null pointer exception error",
      inferResults,
      doesNotContain(
        NULL_DEREFERENCE,
        HashMapModelTest,
        "containsIntegerTwiceThenGetTwice")
    );
  }

  @Test
  public void whenInferRunsOnGetOneIntegerWithoutCheckThenNPEIsFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
      "Results should contain null pointer exception error",
      inferResults,
      contains(
        NULL_DEREFERENCE,
        HashMapModelTest,
        "getOneIntegerWithoutCheck")
    );
  }

  @Test
  public void whenInferRunsOnGetTwoIntegersWithOneCheckThenNPEIsFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
      "Results should contain null pointer exception error",
      inferResults,
      contains(
        NULL_DEREFERENCE,
        HashMapModelTest,
        "getTwoIntegersWithOneCheck")
    );
  }

  @Test
  public void whenInferRunsOnGetOrCreateIntegerThenNPEIsNotFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
      "Results should not contain null pointer exception error",
      inferResults,
      doesNotContain(
        NULL_DEREFERENCE,
        HashMapModelTest,
        "getOrCreateInteger")
    );
  }

  @Test
  public void whenInferRunsOnGetOrCreateIntegerThenDerefThenNPEIsNotFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
      "Results should not contain null pointer exception error",
      inferResults,
      doesNotContain(
        NULL_DEREFERENCE,
        HashMapModelTest,
        "getOrCreateIntegerThenDeref")
    );
  }

}
