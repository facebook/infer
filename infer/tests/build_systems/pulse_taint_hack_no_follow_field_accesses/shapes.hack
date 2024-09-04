// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

namespace Shapes;

class SensitiveClass {
  public string $sensitiveField = "42";

  public function getId(): string {
    return $this->sensitiveField;
  }
}

class ShapeLogger {
  const type TSchemaShape = shape(
    'msg' => string,
    ?'debug_data' => ?string,
  );

  public static function logData(this::TSchemaShape $data): void {
    self::taintSink($data);
  }

  public static function taintSink(mixed $data): void {}
}

class C1 {
  public function passViaShapeOk(SensitiveClass $sc): void {
    ShapeLogger::logData(
      shape('msg' => 'Oh-oh', 'debug_data' => $sc->sensitiveField),
    );
  }

  public function passViaShapeGetBad(SensitiveClass $sc): void {
    $s = shape('msg' => 'Oh-oh', 'debug_data' => $sc->sensitiveField);
    ShapeLogger::taintSink($s['debug_data']);
  }

  public function passUnrelatedViaShapeGetOk(SensitiveClass $sc): void {
    $s = shape('msg' => 'Oh-oh', 'debug_data' => $sc->sensitiveField);
    ShapeLogger::taintSink($s['msg']);
  }

  public function passViaUnknownOk(SensitiveClass $sc): void {
    $data = unknown($sc);
    ShapeLogger::logData(shape('msg' => 'Oh-oh', 'debug_data' => $data));
  }

  public function passViaShapeAndUnknownOk(SensitiveClass $sc): void {
    $data = unknown(shape("sc" => $sc));
    ShapeLogger::logData(shape('msg' => 'Oh-oh', 'debug_data' => $data));
  }
}
