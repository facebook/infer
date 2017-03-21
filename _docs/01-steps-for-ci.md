---
docid: steps-for-ci
title: Recommended flow for CI
layout: docs
permalink: /docs/steps-for-ci.html
---

When you would like to run more then one analyzer, its better to separate analyze and capture phase, so that output of capture phase can be shared.

Below is an example for CI script which runs both infer and eradicate analyzer by sharing the capture phase on an android project.


### Android Gradle

```bash

git diff --name-only origin/<FEATURE_BRANCH>..origin/<MASTER_BRANCH> > index.txt

infer -a capture -- ./gradlew --offline assembleDebug

infer --fail-on-issue -a infer --changed-files-index ./index.txt -- analyze -- ./gradlew --offline assembleDebug

infer --fail-on-issue -a eradicate --changed-files-index ./index.txt -- analyze -- ./gradlew --offline assembleDebug

```

In the above example,

  - We are using git to find the changed files `git diff --name-only`
  - Run capture phase, output of capture phase will be shared by subsequent analyse phases
  - Run analyze phase only for the changed files `-a infer --changed-files-index ./index.txt -- analyze`
