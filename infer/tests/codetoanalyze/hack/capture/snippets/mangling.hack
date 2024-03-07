// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

class KeywordMangling {

  public function __construct(
    public int $declare,
    public int $define,
    public int $extends,
    public int $false,
    public int $float,
    public int $global,
    public int $handlers,
    public int $int,
    public int $jmp,
    public int $load,
    public int $local,
    public int $null,
    public int $prune,
    public int $ret,
    public int $store,
    public int $then,
    public int $throw,
    public int $true,
    public int $type,
    public int $unreachable,
    public int $void,
  ) {}
}

class IdentMangling {

  public function __construct(
    public int $n0,
    public int $n10,
    public int $n0123123,
  ) {}
}
