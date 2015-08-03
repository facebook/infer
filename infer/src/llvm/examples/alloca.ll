; Allocate stack variable using alloca instruction
define i32 @main() {
  %a = alloca i32
  ret i32 0
}
