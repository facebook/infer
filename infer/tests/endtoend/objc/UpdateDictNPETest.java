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
import static utils.matchers.ResultContainsExactly.containsExactly;
import static utils.matchers.ResultContainsLineNumbers.containsLines;
import static utils.matchers.ResultContainsNoErrorInMethod.doesNotContain;
import static utils.matchers.ResultContainsErrorInMethod.contains;

import com.google.common.collect.ImmutableList;

import org.junit.BeforeClass;
import org.junit.ClassRule;
import org.junit.Test;

import java.io.IOException;

import utils.DebuggableTemporaryFolder;
import utils.InferException;
import utils.InferResults;
import utils.InferRunner;

public class UpdateDictNPETest {

  public static final String NPE_FILE = "infer/tests/codetoanalyze/objc/errors/npe/UpdateDict.m";

  private static ImmutableList<String> inferCmdNPD;

  public static final String NULL_DEREFERENCE = "NULL_DEREFERENCE";

  @ClassRule
  public static DebuggableTemporaryFolder folderNPD = new DebuggableTemporaryFolder();

  @BeforeClass
  public static void runInfer() throws InterruptedException, IOException {
    inferCmdNPD = InferRunner.createiOSInferCommandWithMLBuckets(
        folderNPD,
        NPE_FILE,
        "cf",
        true);
  }

  @Test
  public void nullDereferenceTest() throws InterruptedException, IOException, InferException {
    InferResults inferResults = InferRunner.runInferC(inferCmdNPD);
    String[] procedures = {
        "update_dict_with_key_null",
        "update_array_with_null",
        "add_nil_to_array",
        "insert_nil_in_array",
        "add_nil_in_dict",
        "nullable_NSDictionary_objectForKey",
        "nullable_NSDictionary_objectForKeyedSubscript",
        "nullable_NSMapTable_objectForKey"
    };
    assertThat(
        "Results should contain null pointer dereference error",
        inferResults,
        containsExactly(
            NULL_DEREFERENCE,
            NPE_FILE,
            procedures
        )
    );
  }


}
