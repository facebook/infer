// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

namespace TestModels;

class C {
  public mixed $data;
}

function returnNullC(): C {
  return new C();
}

function returnNonnullC(): C {
  return new C();
}

function getNullCFromSil(): C {
  return new C();
}
