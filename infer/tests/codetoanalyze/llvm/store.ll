; Copyright (c) 2015 - present Facebook, Inc.
; All rights reserved.
;
; This source code is licensed under the BSD style license found in the
; LICENSE file in the root directory of this source tree. An additional grant
; of patent rights can be found in the PATENTS file in the same directory.

; Function with store instruction
define i32 @main() {
  store i32 0, i32* %i
  ret i32 0
}
