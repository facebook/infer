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
import javax.lang.model.util.Elements;

@SupportedAnnotationTypes({ "java.lang.SuppressWarnings" })
public class AnnotationProcessor extends AbstractProcessor {

  private static final String ANNOTATION_ENV_VAR = "INFER_ANNOTATIONS_OUT";

  private String mOutputFilename;

  // map of (classes -> methods in class). an empty set means suppress all
  // warnings on class
  public Map<String, Set<String>> mSuppressMap = new LinkedHashMap<String, Set<String>>();

  // total number of classes/methods with a SuppressWarnings annotation
  private int mNumToSuppress = 0;

  // print a comma between all objects except for the last one
  private void outputCommaIfNotLast(PrintWriter out, int elemCount) {
      if (elemCount == mNumToSuppress) {
        out.println("");
      } else {
        out.println(",");
    }
  }

  // output a method to suppress in JSON format
  // clearly, we should be using an existing JSON output library like Jackson here. however, we
  // can't do this because we do not want to add Jackson (or another JSON library) to the classpath
  // of the Java program we are building (along with the JAR for this processor). the reason is that
  // if there is a different version of the JSON parser library somewhere in the classpath for the
  // project, it could cause *very* strange problems. instead, we rolled our own library to avoid
  // introducing additional dependencies
  private void outputMethod(PrintWriter out, String clazz, String method, int elemCount) {
    String TAB1 = "  ";
    String TAB2 = TAB1 + TAB1;
    out.println(TAB1 + "{");
    out.println(TAB2 +"\"language\": \"Java\",");
    out.print(TAB2 + "\"class\": \"" + clazz + "\"");
    if (method != null) {
      out.println(",");
      out.println(TAB2 + "\"method\": \"" + method + "\"");
    } else {
      out.println();
    }
    out.print(TAB1 + "}");
    outputCommaIfNotLast(out, elemCount);
  }

  // output a class to suppress in JSON format
  private void outputClass(PrintWriter out, String clazz, int elemCount) {
    outputMethod(out, clazz, null, elemCount);
  }

  // write the methods/classes to suppress to .inferconfig-style JSON
  private void exportSuppressMap() throws FileNotFoundException, IOException {
    Map<String, String> env = System.getenv();
    if (env.get(ANNOTATION_ENV_VAR) == null) {
      throw new RuntimeException("Env variable INFER_ANNOTATIONS_OUT not set");
    } else {
      mOutputFilename = env.get(ANNOTATION_ENV_VAR);
    }

    // output .inferconfig format file in JSON
    try (PrintWriter out = new PrintWriter(mOutputFilename)) {
      int elemCount = 0;
      out.println("{ \"suppress_procedures\": [");
      for (Map.Entry<String, Set<String>> entry : mSuppressMap.entrySet()) {
        String clazz = entry.getKey();
        Set<String> methods = entry.getValue();
        if (methods.isEmpty()) { // empty set of methods means annotation is on class
          outputClass(out, clazz, ++elemCount);
        } else {
          for (String method : methods) {
            outputMethod(out, clazz, method, ++elemCount);
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
  public boolean process(Set<? extends TypeElement> annotations, RoundEnvironment env) {
    Elements elements = processingEnv.getElementUtils();
    for (TypeElement te : annotations) {
      for (Element e : env.getElementsAnnotatedWith(te)) {
        SuppressWarnings annot = e.getAnnotation(SuppressWarnings.class);
        if (shouldProcess(annot)) {
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

    if (env.processingOver() && mNumToSuppress > 0) {
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
