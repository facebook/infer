/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package com.facebook.infer.annotprocess;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;

import javax.annotation.processing.AbstractProcessor;
import javax.annotation.processing.RoundEnvironment;
import javax.annotation.processing.SupportedAnnotationTypes;
import javax.annotation.processing.SupportedOptions;
import javax.lang.model.SourceVersion;
import javax.lang.model.element.Element;
import javax.lang.model.element.ExecutableElement;
import javax.lang.model.element.TypeElement;
import javax.lang.model.util.Elements;

@SupportedOptions({ "SuppressWarningsOutputFilename" })
@SupportedAnnotationTypes({ "java.lang.SuppressWarnings" })
public class CollectSuppressWarnings extends AbstractProcessor {

  private static final String OUTPUT_FILENAME_OPTION = "SuppressWarningsOutputFilename";

  // map of (classes -> methods in class). an empty set means suppress all warnings on class
  public Map<String, Set<String>> mSuppressMap = new LinkedHashMap<String, Set<String>>();

  // total number of classes/methods with a SuppressWarnings annotation
  private int mNumToSuppress = 0;

  // write the methods/classes to suppress to .inferconfig-style JSON
  private void exportSuppressMap() throws IOException {

    Map<String, String> options = processingEnv.getOptions();
    String mOutputFilename =
      Preconditions.checkNotNull(options.get(OUTPUT_FILENAME_OPTION),
                                 "The filename should be passed from the Infer top-level script");

    // output .inferconfig format file in JSON
    try (PrintWriter out = new PrintWriter(mOutputFilename)) {
      int elemCount = 0;
      out.println("{ \"suppress_warnings\": [");
      for (Map.Entry<String, Set<String>> entry : mSuppressMap.entrySet()) {
        String clazz = entry.getKey();
        Set<String> methods = entry.getValue();
        if (methods.isEmpty()) { // empty set of methods means annotation is on class
          JSONOutputUtils.outputClass(out, clazz, ++elemCount, mNumToSuppress);
        } else {
          for (String method : methods) {
            JSONOutputUtils.outputMethod(out, clazz, method, ++elemCount, mNumToSuppress);
          }
        }
      }
      out.println("] }");
    }
  }

  // we only care about @SuppressWarnings("null") and @SuppressWarnings("infer"); ignore otherwise
  private boolean shouldProcess(SuppressWarnings annot) {
    for (String value : annot.value()) {
      if (value.equals("null") || value.equals("infer")) return true;
    }
    return false;
  }

  // collect all of the SuppressWarnings annotations from the Java source files being compiled
  public boolean process(Set<? extends TypeElement> annotations, RoundEnvironment roundEnv) {

    Elements elements = processingEnv.getElementUtils();
    for (TypeElement te : annotations) {
      for (Element e : roundEnv.getElementsAnnotatedWith(te)) {
        SuppressWarnings annot = e.getAnnotation(SuppressWarnings.class);
        if (annot != null && shouldProcess(annot)) {
          if (e instanceof TypeElement) { // class
            String className = elements.getBinaryName((TypeElement) e).toString();
            mSuppressMap.put(className, Collections.EMPTY_SET);
            mNumToSuppress++;
          } else if (e instanceof ExecutableElement) { // method
            String classname = e.getEnclosingElement().toString();
            java.util.Set<String> suppressMethods = mSuppressMap.get(classname);
            if (suppressMethods != null && suppressMethods.isEmpty()) {
              // empty set means suppress warnings on all methods in class; do nothing
              continue;
            }
            if (suppressMethods == null) {
              suppressMethods = new LinkedHashSet<String>();
            }
            suppressMethods.add(e.getSimpleName().toString());
            mSuppressMap.put(classname, suppressMethods);
            mNumToSuppress++;
          }
        }
      }
    }

    if (roundEnv.processingOver() && mNumToSuppress > 0) {
      try {
        exportSuppressMap();
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
