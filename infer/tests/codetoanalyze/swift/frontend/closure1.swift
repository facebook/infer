class RetainCycleExample {
    var id = 10
    // The closure property
    var closure: (() -> Void) = {}
}


func createObject() -> RetainCycleExample {
    let obj = RetainCycleExample()
    obj.closure = { obj.id = 20 }
    return obj
}

func test() {
    let obj = createObject()
    obj.closure()
}
