---
id: analyzing-android-app
title: Analyzing an Android app
layout: docs
permalink: /docs/analyzing-android-app.html
prev: getting-started.html
next: analyzing-ios-app.html
---
## Analyzing a gradle project

```bash
$> gradle clean
$> infer -- gradle <gradle task, i.e. "build">
```
## Analyzing a buck project


```bash
$> buck clean
$> infer -- buck <buck target>
```

## View results
A list of bugs will appear in the standard output.
