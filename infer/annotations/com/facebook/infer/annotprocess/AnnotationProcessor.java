/*
* Copyright (c) 2015 - present Facebook, Inc.
* All rights reserved.
*
* This source code is licensed under the BSD style license found in the
* LICENSE file in the root directory of this source tree. An additional grant
* of patent rights can be found in the PATENTS file in the same directory.
*/

package com.facebook.infer.annotprocess;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;

import javax.annotation.processing.AbstractProcessor;
import javax.annotation.processing.RoundEnvironment;
import javax.annotation.processing.SupportedAnnotationTypes;
import javax.lang.model.SourceVersion;
import javax.lang.model.element.Element;
import javax.lang.model.element.ExecutableElement;
import javax.lang.model.element.TypeElement;

@SupportedAnnotationTypes({ "java.lang.SuppressWarnings" })
public class AnnotationProcessor extends AbstractProcessor {

  private static final String ANNOTATION_ENV_VAR = "INFER_ANNOTATIONS_OUT";

  private String mOutputFilename = "suppress_warnings_map.txt";

  // map of (classes -> methods in class). an empty set means suppress all
  // warnings on class
  public Map<String, Set<String>> mSuppressMap = new LinkedHashMap<String, Set<String>>();

  private void exportSuppressMap() throws FileNotFoundException, IOException {
    Map<String, String> env = System.getenv();
    if (env.get(ANNOTATION_ENV_VAR) == null) {
      throw new RuntimeException("Env variable INFER_ANNOTATIONS_OUT not set");
    } else {
      mOutputFilename = env.get(ANNOTATION_ENV_VAR);
    }
    try (PrintWriter out = new PrintWriter(mOutputFilename)) {
      for (Map.Entry<String, Set<String>> entry : mSuppressMap.entrySet()) {
        out.write(entry.getKey() + " " + entry.getValue());
      }
    }
  }

  public boolean process(Set<? extends TypeElement> annotations, RoundEnvironment env) {
    for (TypeElement te : annotations) {
      for (Element e : env.getElementsAnnotatedWith(te)) {
        if (e instanceof TypeElement) { // class
          String className = ((TypeElement) e).getQualifiedName().toString();
          mSuppressMap.put(className, Collections.EMPTY_SET);
        } else if (e instanceof ExecutableElement) { // method

          String classname = e.getEnclosingElement().toString();
          java.util.Set<String> suppressMethods = mSuppressMap.get(classname);
          if (suppressMethods != null && suppressMethods.isEmpty()) {
            // empty set means suppress warnings on all methods in class; do
            // nothing
            continue;
          }
          if (suppressMethods == null) {
            suppressMethods = new LinkedHashSet<String>();
          }
          suppressMethods.add(e.toString());
          mSuppressMap.put(classname, suppressMethods);
        }
      }
    }

    if (env.processingOver()) {
      try {
        exportSuppressMap();
      } catch (FileNotFoundException e) {
        throw new RuntimeException(e);
      } catch (IOException e) {
        throw new RuntimeException(e);
      }
    }
    return false;
  }

  @Override
  public SourceVersion getSupportedSourceVersion() {
    return SourceVersion.latestSupported();
  }

}
