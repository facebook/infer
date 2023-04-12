// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

function directCrossFileFlowBad(): void {
  $tainted = OuterFile::taintSource();
  OuterFile::taintSink($tainted);
}

function directCrossFileFlowOk(int $untainted): void {
  OuterFile::taintSink($untainted);
}

function indirectCrossFileFlowBad(): void {
  $tainted = OuterFile::tainted();
  OuterFile::taintSink($tainted);
}

function indirectCrossFileFlowOk(): void {
  $untainted = OuterFile::untainted();
  OuterFile::taintSink($untainted);
}

function inheritanceCrossFileFlowBad(): void {
  // The taint config specifies class' superclass as a source
  // Matching against this spec works only when we have a global tenv
  $tainted = OuterFile::superTaintSource();
  OuterFile::taintSink($tainted);
}
