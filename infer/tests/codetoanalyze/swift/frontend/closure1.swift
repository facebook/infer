class RetainCycleExample {
    var id = 10
    // The closure property
    var closure: (() -> Void) = {}
}


func test(_ obj : RetainCycleExample) {
    obj.closure()
}
