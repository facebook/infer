// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

namespace ControlFlow;


function typeCheckDoesntConfuseTheAnalysis_maintainsTaint_Bad(mixed $arg1, SensitiveClass $sc) {
  if ($arg1 is Foo) {
    \Level1\taintSink($sc);
  }
}
