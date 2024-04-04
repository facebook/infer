// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

class Attributes {

  <<Stuff(shape('required' => true))>> public ?string $name;

  public function __construct(?string $name = null): void {
    $this->name = $name;
  }

}
