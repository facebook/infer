// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

namespace DictTests;

class Main {

  public function init_and_load_bad(int $u, int $v, int $w): void {
    $tainted = \Level1\taintSource();

    $t1 = dict['a' => $u, 'b' => $v];

    if ($t1['a'] == $u && $t1['b'] == $v) {
      \Level1\taintSink($tainted);
    }
  }

  public function init_and_load_good(int $u, int $v, int $w): void {
    $tainted = \Level1\taintSource();

    $t1 = dict['a' => $u, 'b' => $v];

    if ($t1['a'] != $u || $t1['b'] != $v) {
      \Level1\taintSink($tainted);
    }
  }

  public function copy_on_write_bad(int $u, int $v, int $w): void {
    $tainted = \Level1\taintSource();

    $t1 = dict['a' => $u, 'b' => $v];

    $t2 = $t1;

    $t2['a'] = $w;

    if ($t1['a'] == $u && $t2['a'] == $w && $t1['b'] == $v && $t2['b'] == $v) {
      \Level1\taintSink($tainted);
    }
  }

  public function copy_on_write_good(int $u, int $v, int $w): void {
    $tainted = \Level1\taintSource();

    $t1 = dict['a' => $u, 'b' => $v];

    $t2 = $t1;

    $t2['a'] = $w;

    if ($t1['a'] != $u || $t2['a'] != $w || $t1['b'] != $v || $t2['b'] != $v) {
      \Level1\taintSink($tainted);
    }
  }

  public function multidim_copy_on_write_bad(
    int $u1,
    int $v1,
    int $u2,
    int $v2,
    int $w,
  ): void {
    $tainted = \Level1\taintSource();

    $t1 = dict[
      'level1' => dict['a' => $u1, 'b' => $v1],
      'level2' => dict['a' => $u2, 'b' => $v2],
    ];

    $t2 = $t1['level2'];

    $t1['level2']['a'] = $w;

    if (
      $t1['level1']['a'] == $u1 &&
      $t1['level1']['b'] == $v1 &&
      $t1['level2']['a'] == $w &&
      $t2['a'] == $u2 &&
      $t1['level2']['b'] == $v2 &&
      $t2['b'] == $v2
    ) {
      \Level1\taintSink($tainted);
    }
  }

  public function multidim_copy_on_write_good(
    int $u1,
    int $v1,
    int $u2,
    int $v2,
    int $w,
  ): void {
    $tainted = \Level1\taintSource();

    $t1 = dict[
      'level1' => dict['a' => $u1, 'b' => $v1],
      'level2' => dict['a' => $u2, 'b' => $v2],
    ];

    $t2 = $t1['level2'];

    $t1['level2']['a'] = $w;

    if ($t1['level1']['a'] != $u1) {
      \Level1\taintSink($tainted);
    }
    if ($t1['level1']['b'] != $v1) {
      \Level1\taintSink($tainted);
    }
    if ($t1['level2']['a'] != $w) {
      \Level1\taintSink($tainted);
    }
    if ($t2['a'] != $u2) {
      \Level1\taintSink($tainted);
    }
    if ($t1['level2']['b'] != $v2) {
      \Level1\taintSink($tainted);
    }
    if ($t2['b'] != $v2) {
      \Level1\taintSink($tainted);
    }
  }

  public function copy_on_write_no_dynamic_type_bad(
    dict<string, int> $dict,
  ): void {
    $tainted = \Level1\taintSource();
    $dict['a'] = 1;
    \Level1\taintSink($tainted);
  }

  public static function init_with_call(int $u, int $v): dict<string, int> {
    return dict['a' => $u, 'b' => $v];
  }

  public function init_with_call_and_load_bad(int $u, int $v, int $w): void {
    $tainted = \Level1\taintSource();

    $t1 = self::init_with_call($u, $v);

    if ($t1['a'] == $u && $t1['b'] == $v) {
      \Level1\taintSink($tainted);
    }
  }

  public function init_with_call_and_load_good(int $u, int $v, int $w): void {
    $tainted = \Level1\taintSource();

    $t1 = self::init_with_call($u, $v);

    if ($t1['a'] != $u || $t1['b'] != $v) {
      \Level1\taintSink($tainted);
    }
  }

  public static async function gen_async_init_with_call(
    int $u,
    int $v,
  ): Awaitable<dict<string, int>> {
    return dict['a' => $u, 'b' => $v];
  }

  public async function init_with_async_call_and_load_bad(
    int $u,
    int $v,
    int $w,
  ): Awaitable<void> {
    $tainted = \Level1\taintSource();

    $t1 = await self::gen_async_init_with_call($u, $v);

    if ($t1['a'] == $u && $t1['b'] == $v) {
      \Level1\taintSink($tainted);
    }
  }

  public async function init_with_async_call_and_load_good(
    int $u,
    int $v,
    int $w,
  ): Awaitable<void> {
    $tainted = \Level1\taintSource();

    $t1 = await self::gen_async_init_with_call($u, $v);

    if ($t1['a'] != $u || $t1['b'] != $v) {
      \Level1\taintSink($tainted);
    }
  }

  public function vec_of_dict_bad(): void {
    $tainted = \Level1\taintSource();
    $shape = shape('x' => $tainted, 'y' => 0);
    $vec = vec[$shape];
    \Level1\taintSink($vec[0]['x']);
  }

  public function vec_of_dict_good(): void {
    $tainted = \Level1\taintSource();
    $shape = shape('x' => $tainted, 'y' => 0);
    $vec = vec[$shape];
    \Level1\taintSink($vec[0]['y']);
  }

  public function vec_of_vec_bad(): void {
    $tainted = \Level1\taintSource();
    $vec = vec[vec[$tainted]];
    \Level1\taintSink($vec[0][0]);
  }

  public function FP_vec_of_vec_good(): void {
    $tainted = \Level1\taintSource();
    $vec = vec[vec[$tainted], vec[0]];
    \Level1\taintSink($vec[1][0]);
  }

}
