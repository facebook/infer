// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

namespace ExtraModels;

function derefNullFromHackModelBad(): mixed {
  return \TestModels\returnNullC()->data;
}

function derefNonnullFromHackModelOk(): mixed {
  return \TestModels\returnNonnullC()->data;
}

function derefNullFromSilModelBad(): mixed {
  return \TestModels\getNullCFromSil()->data;
}
