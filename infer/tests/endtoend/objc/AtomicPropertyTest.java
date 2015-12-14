/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package endtoend.objc;

import static org.hamcrest.MatcherAssert.assertThat;
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

public class AtomicPropertyTest {

  public static final String FILE =
      "infer/tests/codetoanalyze/objc/warnings/atomic_prop.m";

  private static ImmutableList<String> inferCmd;

  public static final String DIRECT_ATOMIC_PROPERTY_ACCESS = "DIRECT_ATOMIC_PROPERTY_ACCESS";

  @ClassRule
  public static DebuggableTemporaryFolder folder =
      new DebuggableTemporaryFolder();

  @BeforeClass
  public static void runInfer() throws InterruptedException, IOException {
    inferCmd = InferRunner.createObjCInferCommand(
        folder,
        FILE);
  }

  @Test
  public void whenInferRunsOnAtomicProperty()
      throws InterruptedException, IOException, InferException {
    InferResults inferResults = InferRunner.runInferObjC(inferCmd);
    assertThat(
        "Results should contain a direct atomic property access",
        inferResults,
        containsExactly(
            DIRECT_ATOMIC_PROPERTY_ACCESS,
            FILE,
            new String[]{
                "writeQ:",
                "readQ",
                "bla"
            }
        )
    );
  }

}
