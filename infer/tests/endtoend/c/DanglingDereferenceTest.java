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
import static utils.matchers.ResultContainsLineNumbers.containsLines;
import static utils.matchers.ResultContainsErrorInMethod.contains;
import org.junit.BeforeClass;
import org.junit.Test;

import java.io.IOException;

import utils.InferException;
import utils.InferResults;
import utils.InferRunner;

public class DanglingDereferenceTest {

    public static final String SOURCE_FILE =
    "dangling_deref/dpd.c";

    public static final String DANGLING_POINTER_DEREFERENCE = "DANGLING_POINTER_DEREFERENCE";

    private static InferResults inferResults;

    @BeforeClass
    public static void runInfer() throws InterruptedException, IOException {
        inferResults = InferResults.loadCInferResults(
                                                      DanglingDereferenceTest.class,
                                                      SOURCE_FILE);
    }


    @Test
    public void DanglingDereferenceTest1() throws InterruptedException, IOException, InferException {
        assertThat(
                   "Results should contain dangling pointer dereference error",
                   inferResults,
                   contains(
                            DANGLING_POINTER_DEREFERENCE,
                            SOURCE_FILE,
                            "dpd"));
    }

    @Test
    public void DanglingDereferenceTest2() throws InterruptedException, IOException, InferException {
        assertThat(
                   "Results should contain dangling pointer dereference error",
                   inferResults,
                   contains(
                            DANGLING_POINTER_DEREFERENCE,
                            SOURCE_FILE,
                            "intraprocdpd"));
    }



}
