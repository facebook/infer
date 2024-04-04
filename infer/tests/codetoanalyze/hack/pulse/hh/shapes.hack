// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

namespace Shapes;

function unknown(mixed $sc): string {
  return "42";
}

class Unknown {
  public static function unknown(mixed $sc): shape('field' => string) {
    return shape('field' => "42");
  }
}
