/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

function dict_argument(dict<string, int> $d): int {
  return $d['bye'];
}

function call_dict_argument_preexiting(): (function(): int) {
  return () ==> dict_argument(dict['hi' => 42]);
}
