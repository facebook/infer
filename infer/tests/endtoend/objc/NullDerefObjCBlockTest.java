/*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package endtoend.objc;

import static org.hamcrest.MatcherAssert.assertThat;
import static utils.matchers.ResultContainsErrorInMethod.contains;
import static utils.matchers.ResultContainsOnlyTheseErrors.containsOnly;

import com.google.common.collect.ImmutableList;

import org.junit.BeforeClass;
import org.junit.ClassRule;
import org.junit.Test;

import java.io.IOException;

import utils.DebuggableTemporaryFolder;
import utils.InferException;
import utils.InferResults;
import utils.InferRunner;

public class NullDerefObjCBlockTest {

  public static final String BLOCK_FILE =
      "infer/tests/codetoanalyze/objc/errors/npe/block.m";


  private static ImmutableList<String> inferCmdBlock;

  private static final String PARAMETER_NOT_NULL_CHECKED = "PARAMETER_NOT_NULL_CHECKED";
  public static final String NULL_DEREFERENCE = "NULL_DEREFERENCE";
  public static final String IVAR_NOT_NULL_CHECKED = "IVAR_NOT_NULL_CHECKED";

  @ClassRule
  public static DebuggableTemporaryFolder folderBlock =
      new DebuggableTemporaryFolder();

  @BeforeClass
  public static void runInfer() throws InterruptedException, IOException {
    inferCmdBlock = InferRunner.createObjCInferCommandWithMLBuckets(
        folderBlock,
        BLOCK_FILE,
        "cf",
        false);

  }

  @Test
  public void whenInferRunsOnAClass1ThenNpeIsFound()
      throws InterruptedException, IOException, InferException {
    InferResults inferResults = InferRunner.runInferObjC(inferCmdBlock);
    assertThat(
        "Results should contain parameter not null checked error",
        inferResults,
        contains(
            PARAMETER_NOT_NULL_CHECKED,
            BLOCK_FILE,
            "doSomethingThenCallback:"
        )
    );
  }

  @Test
  public void whenInferRunsOnAClass2ThenNpeIsFound()
      throws InterruptedException, IOException, InferException {
    InferResults inferResults = InferRunner.runInferObjC(inferCmdBlock);
    assertThat(
        "Results should contain null pointer dereference error",
        inferResults,
        contains(
            NULL_DEREFERENCE,
            BLOCK_FILE,
            "foo"
        )
    );
  }

  @Test
  public void whenInferRunsOnAClass3ThenNpeIsFound()
      throws InterruptedException, IOException, InferException {
    InferResults inferResults = InferRunner.runInferObjC(inferCmdBlock);
    assertThat(
        "Results should contain null pointer dereference error",
        inferResults,
        contains(
            NULL_DEREFERENCE,
            BLOCK_FILE,
            "foo3:"
        )
    );
  }

  @Test
  public void whenInferRunsOnAClass4ThenNpeIsFound()
      throws InterruptedException, IOException, InferException {
    InferResults inferResults = InferRunner.runInferObjC(inferCmdBlock);
    assertThat(
        "Results should contain null pointer dereference error",
        inferResults,
        contains(
            NULL_DEREFERENCE,
            BLOCK_FILE,
            "foo4:"
        )
    );
  }


  @Test
  public void whenInferRunsOnAClass5ThenNpeIsFound()
      throws InterruptedException, IOException, InferException {
    InferResults inferResults = InferRunner.runInferObjC(inferCmdBlock);
    assertThat(
        "Results should contain ivar not nullable error",
        inferResults,
        contains(
            IVAR_NOT_NULL_CHECKED,
            BLOCK_FILE,
            "foo7"
        )
    );
  }
}
