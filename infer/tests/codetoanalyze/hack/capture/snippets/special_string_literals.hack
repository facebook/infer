// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

class SpecialStringLiterals {

  <<Stuff(shape('required' => true))>> public ?string $name;

  public function __construct(?string $name = null): void {
    $this->name = $name;
  }

  private static function print(mixed $value)[]: string {
    if ($value is null) {
      return 'null';
    } else if ($value is string) {
      return "\"$value\"";
    }
    return "";
  }

}
