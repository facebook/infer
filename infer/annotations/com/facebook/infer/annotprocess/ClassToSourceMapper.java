/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package com.facebook.infer.annotprocess;

import com.sun.source.tree.CompilationUnitTree;
import com.sun.source.util.Trees;
import com.sun.source.util.TreePath;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.annotation.processing.AbstractProcessor;
import javax.annotation.processing.RoundEnvironment;
import javax.annotation.processing.SupportedAnnotationTypes;
import javax.annotation.processing.SupportedOptions;
import javax.lang.model.SourceVersion;
import javax.lang.model.element.Element;
import javax.lang.model.element.TypeElement;
import javax.lang.model.util.Elements;

@SupportedOptions({ "classSourceMapOutputFilename" })
// this says: "process all classes, even ones without any annotations"
@SupportedAnnotationTypes({ "*" })
public class ClassToSourceMapper extends AbstractProcessor {

  // map from class name -> absolute path to source file
  // e.g., com.example.MyClass -> = /Users/me/MyClass.Java
  // note that this map does not contain inner or anonymous classes
  private Map<String, String> mClassSourceMap = new LinkedHashMap<String, String>();

  private static final String DEFAULT_OUTPUT_FILENAME = "classSourceMap.json";

  private static final String OUTPUT_FILENAME_OPTION = "classSourceMapOutputFilename";

  private void exportClassSourceMap(String filename) throws FileNotFoundException, IOException {
    try (PrintWriter out = new PrintWriter(filename)) {
      out.println("{");
      int elemCount = 0;
      int elemMax = mClassSourceMap.size();
      for (Map.Entry<String, String> entry : mClassSourceMap.entrySet()) {
        String className = entry.getKey();
        String sourcePath = entry.getValue();
        JSONOutputUtils.outputClassSourcePair(out, className, sourcePath, ++elemCount, elemMax);
      }
      out.println("}");
    }
  }

  private void makeClassSourceMap(RoundEnvironment env) {
    Set<? extends Element> rootEnv = env.getRootElements();
    Trees trees = Trees.instance(processingEnv);
    for (Element e : rootEnv) {
      if (e instanceof TypeElement) { // class or interface
        TreePath path = trees.getPath(e);
        TypeElement typeElem = (TypeElement) e;
        CompilationUnitTree compilationUnit = path.getCompilationUnit();
        String absoluteSourcePath = compilationUnit.getSourceFile().toUri().getPath();
        // map a class name to its source file. this will only capture top-level class names; inner
        // classes (anonymous and otherwise) are dealt with later reading each top-level class's
        // list of inner classes and mapping each one to the source file of the parent class
        mClassSourceMap.put(typeElem.getQualifiedName().toString(), absoluteSourcePath);
      }
    }
  }

  @Override
  public boolean process(Set<? extends TypeElement> annotations, RoundEnvironment env) {
    makeClassSourceMap(env);
    if (env.processingOver()) {
      try {
        Map<String,String> options = processingEnv.getOptions();
        String outputFilename = options.get(OUTPUT_FILENAME_OPTION);
        if (outputFilename == null) {
          outputFilename = DEFAULT_OUTPUT_FILENAME;
        }
        exportClassSourceMap(outputFilename);
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
