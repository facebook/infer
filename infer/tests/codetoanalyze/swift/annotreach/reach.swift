// Global functions
func sink() {
}

func sanitizer() {
  sink()
}

func source_Bad() {
  sink()
}

func source_Ok() {
  sanitizer()
}

// Functions in classes
class MyClass {
  func sink() {
  }

  func source_Bad() {
    sink()
  }
}

// Functions on receiver objects
class AnotherClass {
  func not_a_sou_rce_Ok(obj: MyClass) {
    obj.sink()
  }

  func source_Bad(obj: MyClass) {
    obj.sink()
  }
}
