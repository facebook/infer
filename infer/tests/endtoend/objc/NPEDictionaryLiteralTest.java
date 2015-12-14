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
import static utils.matchers.ResultContainsNoErrorInMethod.doesNotContain;
import static utils.matchers.ResultContainsExactly.containsExactly;

import com.google.common.collect.ImmutableList;

import org.junit.BeforeClass;
import org.junit.ClassRule;
import org.junit.Test;

import java.io.IOException;

import utils.DebuggableTemporaryFolder;
import utils.InferException;
import utils.InferResults;
import utils.InferRunner;

public class NPEDictionaryLiteralTest {

  public static final String PREMATURE_NIL_FILE =
      "infer/tests/codetoanalyze/objc/errors/npe/nil_in_dictionary_literal.m";

  private static ImmutableList<String> inferCmd;

  public static final String NULL_DEREFERENCE = "NULL_DEREFERENCE";

  @ClassRule
  public static DebuggableTemporaryFolder folderNPD = new DebuggableTemporaryFolder();

  @BeforeClass
  public static void runInfer() throws InterruptedException, IOException {
    inferCmd = InferRunner.createObjCInferCommandWithMLBuckets(
        folderNPD,
        PREMATURE_NIL_FILE,
        "cf",
        true);
  }

  @Test
  public void whenInferRunsOnTestThenNoNPENotFound()
      throws InterruptedException, IOException, InferException {
    InferResults inferResults = InferRunner.runInferObjC(inferCmd);

    assertThat(
        "NPE should not be found", inferResults,
        doesNotContain(
            NULL_DEREFERENCE,
            PREMATURE_NIL_FILE,
            "no_problem"));

    String[] expectedNPEProcedures = {
        "nilInDictionaryLiteralKey0",
        "nilInDictionaryLiteralKey1",
        "nilInDictionaryLiteralKey2",
        "nilInDictionaryLiteralKey3",
        "nilInDictionaryLiteralValue0",
        "nilInDictionaryLiteralValue1",
        "nilInDictionaryLiteralValue2",
        "nilInDictionaryLiteralValue3"
    };
    assertThat(
        "Only NPE should be found", inferResults,
        containsExactly(
            NULL_DEREFERENCE,
            PREMATURE_NIL_FILE,
            expectedNPEProcedures));
  }
}
