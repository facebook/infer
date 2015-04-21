---
id: analyzing-ios-app
title: Analyzing an iOS app
layout: docs
permalink: /docs/analyzing-ios-app.html
section: User Guide
section_order: 01
order: 01
---

## Using xcodebuild

```bash
$> xcodebuild -target <target name> -configuration <build configuration> -sdk iphonesimulator clean
$> infer -- xcodebuild -target <target name> -configuration <build configuration> -sdk iphonesimulator
```

## View results

A list of bugs will appear in the standard output.
