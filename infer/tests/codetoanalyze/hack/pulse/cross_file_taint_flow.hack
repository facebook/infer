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

function FN_inheritanceCrossFileFlowBad(): void {
  $tainted = OuterFile::superTaintSource(); // works if OuterFile$static.superTaintSource specified as a source
  OuterFile::taintSink($tainted);
}
