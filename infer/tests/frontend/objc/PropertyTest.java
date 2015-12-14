/*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package frontend.objc;

import static org.hamcrest.MatcherAssert.assertThat;
import static utils.matchers.DotFilesEqual.dotFileEqualTo;

import com.google.common.collect.ImmutableList;

import org.junit.Rule;
import org.junit.Test;

import java.io.File;
import java.io.IOException;

import utils.DebuggableTemporaryFolder;
import utils.InferException;
import utils.InferRunner;


public class PropertyTest {

  @Rule
  public DebuggableTemporaryFolder folder = new DebuggableTemporaryFolder();


  @Test
  public void whenCaptureRunOnPropertyThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    String property_expr =
        "infer/tests/codetoanalyze/" +
            "objc/frontend/property/main_car.m";

    String property_dotty =
        "infer/tests/codetoanalyze/" +
            "objc/frontend/property/main_car.dot";

    ImmutableList<String> inferCmd =
        InferRunner.createObjCInferCommandFrontend(folder, property_expr);
    File newDotFile = InferRunner.runInferFrontend(inferCmd);
    assertThat(
        "In the capture of " + property_expr +
            " the dotty files should be the same.",
        newDotFile, dotFileEqualTo(property_dotty));
  }

  @Test
  public void whenCaptureRunOnDynamicPropertyThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    String property_expr =
        "infer/tests/codetoanalyze/" +
            "objc/frontend/property/aclass.m";

    String property_dotty =
        "infer/tests/codetoanalyze/" +
            "objc/frontend/property/aclass.dot";

    ImmutableList<String> inferCmd =
        InferRunner.createObjCInferCommandFrontend(folder, property_expr);
    File newDotFile = InferRunner.runInferFrontend(inferCmd);
    assertThat(
        "In the capture of " + property_expr +
            " the dotty files should be the same.",
        newDotFile, dotFileEqualTo(property_dotty));
  }

  @Test
  public void whenCaptureRunOnPropertyInProtocolThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    String property_expr =
        "infer/tests/codetoanalyze/objc/frontend/" +
            "property_in_protocol/Test.m";

    String property_dotty =
        "infer/tests/codetoanalyze/objc/frontend/" +
            "property_in_protocol/Test.dot";

    ImmutableList<String> inferCmd =
        InferRunner.createObjCInferCommandFrontend(folder, property_expr);
    File newDotFile = InferRunner.runInferFrontend(inferCmd);
    assertThat(
        "In the capture of " + property_expr +
            " the dotty files should be the same.",
        newDotFile, dotFileEqualTo(property_dotty));
  }

  @Test
  public void whenCaptureRunOnPropertyImplSetterThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    String property_expr =
        "infer/tests/codetoanalyze/objc/frontend/" +
            "property/PropertyImplSetter.m";

    String property_dotty =
        "infer/tests/codetoanalyze/objc/frontend/" +
            "property/PropertyImplSetter.dot";

    ImmutableList<String> inferCmd =
        InferRunner.createObjCInferCommandFrontend(folder, property_expr);
    File newDotFile = InferRunner.runInferFrontend(inferCmd);
    assertThat(
        "In the capture of " + property_expr +
            " the dotty files should be the same.",
        newDotFile, dotFileEqualTo(property_dotty));
  }

  @Test
  public void whenCaptureRunOnPropertyAttributesThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    String property_expr =
        "infer/tests/codetoanalyze/objc/frontend/" +
            "property/PropertyAttributes.m";

    String property_dotty =
        "infer/tests/codetoanalyze/objc/frontend/" +
            "property/PropertyAttributes.dot";

    ImmutableList<String> inferCmd =
        InferRunner.createObjCInferCommandFrontend(folder, property_expr);
    File newDotFile = InferRunner.runInferFrontend(inferCmd);
    assertThat(
        "In the capture of " + property_expr +
            " the dotty files should be the same.",
        newDotFile, dotFileEqualTo(property_dotty));
  }

  @Test
  public void whenCaptureRunOnProperty_getterThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    String property_expr =
        "infer/tests/codetoanalyze/objc/frontend/property/Property_getter.m";

    String property_dotty =
        "infer/tests/codetoanalyze/objc/frontend/property/Property_getter.dot";

    ImmutableList<String> inferCmd =
        InferRunner.createObjCInferCommandFrontend(folder, property_expr);
    File newDotFile = InferRunner.runInferFrontend(inferCmd);
    assertThat(
        "In the capture of " + property_expr +
            " the dotty files should be the same.",
        newDotFile, dotFileEqualTo(property_dotty));
  }

  @Test
  public void whenCaptureRunOnPropertyCustomAccessorThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    String property_expr =
        "infer/tests/codetoanalyze/objc/frontend/property/PropertyCustomAccessor.m";

    String property_dotty =
        "infer/tests/codetoanalyze/objc/frontend/property/PropertyCustomAccessor.dot";

    ImmutableList<String> inferCmd =
        InferRunner.createObjCInferCommandFrontend(folder, property_expr);
    File newDotFile = InferRunner.runInferFrontend(inferCmd);
    assertThat(
        "In the capture of " + property_expr +
            " the dotty files should be the same.",
        newDotFile, dotFileEqualTo(property_dotty));
  }



}
