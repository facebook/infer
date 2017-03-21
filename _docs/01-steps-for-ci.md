---
docid: steps-for-ci
title: Recommended flow for CI
layout: docs
permalink: /docs/steps-for-ci.html
---

The recommended flow for CI integration is to determine the modified files, and run the analysis in reactive mode starting from those files. If you would like to run more then one analyzer, it is more efficient to separate the capture phase, so that the result can be used by all the analyzers.


### Android Gradle
The following CI script runs the `infer` and `eradicate` analyzers.

```bash

git diff --name-only origin/<FEATURE_BRANCH>..origin/<MASTER_BRANCH> > index.txt

infer -a capture -- ./gradlew --offline assembleDebug

infer --fail-on-issue -a infer --changed-files-index ./index.txt -- analyze

infer --fail-on-issue -a eradicate --changed-files-index ./index.txt -- analyze

```

Notice that
  - We use git to find the changed files `git diff --name-only`
  - We run capture only once, and the output is kept for the subsequent analyses
  - We run the analyses only for the changed files `--changed-files-index ./index.txt`
