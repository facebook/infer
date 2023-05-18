// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

namespace DictTests;

class Main {

  public function init_and_load_bad(int $u, int $v, int $w) {
    $tainted = \Level1\taintSource();

    $t1 = dict['a' => $u, 'b' => $v];

    if ($t1['a'] == $u && $t1['b'] == $v) {
      \Level1\taintSink($tainted);
    }
  }

  public function init_and_load_good(int $u, int $v, int $w) {
    $tainted = \Level1\taintSource();

    $t1 = dict['a' => $u, 'b' => $v];

    if ($t1['a'] != $u || $t1['b'] != $v) {
      \Level1\taintSink($tainted);
    }
  }

  public function copy_on_write_bad(int $u, int $v, int $w) {
    $tainted = \Level1\taintSource();

    $t1 = dict['a' => $u, 'b' => $v];

    $t2 = $t1;

    $t2['a'] = $w;

    if ($t1['a'] == $u && $t2['a'] == $w && $t1['b'] == $v && $t2['b'] == $v) {
      \Level1\taintSink($tainted);
    }
  }

  public function FP_copy_on_write_good(int $u, int $v, int $w) {
    $tainted = \Level1\taintSource();

    $t1 = dict['a' => $u, 'b' => $v];

    $t2 = $t1;

    $t2['a'] = $w; //currant HackC translation makes copy-on-write hard to track

    if ($t1['a'] != $u || $t2['a'] != $w || $t1['b'] != $v || $t2['b'] != $v) {
      \Level1\taintSink($tainted);
    }
  }
}
