// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

namespace Unknown {
  function FN_basicFlowBad(SensitiveClass $sc): void {
    // Although, we don't have the definition of this method we should still match it against taint
    // configs
    UnknownClass::explicitSinkAllArgs($sc);
  }
}
