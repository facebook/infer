; Function with store instruction
define i32 @main() {
  store i32 0, i32* %i
  ret i32 0
}
