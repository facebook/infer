---
id: steps-for-ci
title: Recommended flow for CI
---

The recommended flow for CI integration is to determine the modified files, and
run the analysis in reactive mode starting from those files. If you would like
to run more than one analyzer, it is more efficient to separate the capture
phase, so that the result can be used by all the analyzers.

### Differential Workflow

Here's how to run infer on two versions of a project and compare the results in
general.

Assume the project uses git, `feature` is the feature branch (the code change
you want to analyze), `main` is the main branch, and `make` builds the
project.

```bash
# go to feature branch if not there already
git checkout feature
# get list of changed files
git diff --name-only origin/feature..origin/main > index.txt
## first run: feature branch
# run infer on the feature branch
infer capture -- make -j 4  # assuming a machine with 4 cores
infer analyze --changed-files-index index.txt
# store the infer report
cp infer-out/report.json report-feature.json
## second run: main branch
git checkout main
# run capture in reactive mode so that previously-captured source files are kept if they are up-to-date
infer capture --reactive -- make -j 4
infer analyze --reactive --changed-files-index index.txt
# compare reports
infer reportdiff --report-current report-feature.json --report-previous infer-out/report.json
```

At the end of this process, "infer-out/differential/" contains three files,
which follow the same format as normal infer JSON reports:

- introduced.json contains the issues found in the feature branch but not in
  main;
- fixed.json contains the issues found in main but not in the feature branch;
- preexisting.json contains the issues found in both branches.

### Example: Android Gradle

The following CI script runs the `infer` and `eradicate` analyzers. Assume again
that `feature` is the feature branch, and `main` is the main branch.

```bash
git diff --name-only origin/feature..origin/main > index.txt
infer capture -- ./gradlew --offline assembleDebug
infer analyze --fail-on-issue --eradicate --changed-files-index ./index.txt
```

Notice that

- We use git to find the changed files `git diff --name-only`
- We run capture only once, and the output is kept for the subsequent analyses
- We run the eradicate analysis alongside the default analyses: `--eradicate`
- We analyze only the changed files `--changed-files-index ./index.txt`
