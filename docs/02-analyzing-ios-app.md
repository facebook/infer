---
id: analyzing-ios-app
title: Analyzing an iOS app
layout: docs
permalink: /docs/analyzing-ios-app.html
prev: analyzing-android-app.html
next: analyzing-c-project.html
---

## Using xcodebuild

```bash
$> xcodebuild -target <target name> -configuration <build configuration> -sdk iphonesimulator clean
$> infer -- xcodebuild -target <target name> -configuration <build configuration> -sdk iphonesimulator
```

## View results

A list of bugs will appear in the standard output.
