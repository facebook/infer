---
id: analyzing-android-app
title: Analyzing an Android app
layout: docs
permalink: /docs/analyzing-android-app.html
section: User Guide
section_order: 01
order: 00
---
## Run infer on a sample android app
Download an example android app that contains some bugs and run infer on it:

```bash
wget localhost:4000/downloads/InferAndroidExample.tar.gz
tar xvfz InferAndroidExample.tar.gz
cd InferAndroidExample
gradle clean
infer -- gradle build
```

Infer will output the list of found bugs:

```bash
InferAndroidExample/app/src/main/java/infer/inferandroidexample/MainActivity.java:20: error: NULL_DEREFERENCE
  [B1] object s last assigned on line 19 could be null and is dereferenced at line 20

InferAndroidExample/app/src/main/java/infer/inferandroidexample/MainActivity.java:37: error: RESOURCE_LEAK
   resource acquired by call to FileOutputStream(...) at line 34 is not released after line 37
```
## Running infer an apps using different build systems

### Gradle

```bash
$> gradle clean && rm -rf infer-out
$> infer -- gradle <gradle task, i.e. "build">
```
### Buck

```bash
$> buck clean && rm -rf infer-out
$> infer -- buck <buck target>
```

### Maven
```bash
$> mvn clean && rm -rf infer-out
$> infer -- mvn <maven target>
```

## Viewing results
A list of bugs will appear on the standard output.
