/*
* Copyright (c) 2015 - present Facebook, Inc.
* All rights reserved.
*
* This source code is licensed under the BSD style license found in the
* LICENSE file in the root directory of this source tree. An additional grant
* of patent rights can be found in the PATENTS file in the same directory.
*/

package com.facebook.infer.annotprocess;

import javax.annotation.processing.*;
import javax.lang.model.element.*;
import javax.tools.*;
import javax.lang.model.SourceVersion;
import java.util.*;

@SupportedAnnotationTypes({"java.lang.SuppressWarnings"})
public class AnnotationProcessor extends AbstractProcessor {

  // map of (classes -> methods in class). an empty set means suppress all warnings on class
  public Map<String,Set<String>> suppressMap = new LinkedHashMap<String,Set<String>>();

  public boolean process(Set<? extends TypeElement> annotations, RoundEnvironment env) {
    for (TypeElement te : annotations) {
      for (Element e : env.getElementsAnnotatedWith(te)) {
        if (e instanceof TypeElement) { // class
          String className = ((TypeElement) e).getQualifiedName().toString();
          suppressMap.put(className, Collections.EMPTY_SET);
        } else if (e instanceof ExecutableElement) { // method
          String clazz = e.getEnclosingElement().toString();
          Set<String> suppressMethods = suppressMap.get(clazz);
          if (suppressMethods == null) {
            suppressMethods = new LinkedHashSet();
            suppressMap.put(clazz, suppressMethods);
          } else if (suppressMethods.isEmpty()) {
            // empty set means suppress warnings on all methods in class; do nothing
            continue;
          }
          suppressMethods.add(clazz);
        }
      }
    }

    if (env.processingOver()) {
      // TODO: write suppressMap to a .inferconfig file on disk
    }
    return false;
  }

  @Override
  public SourceVersion getSupportedSourceVersion() {
    return SourceVersion.latestSupported();
  }

}
