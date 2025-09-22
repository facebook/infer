class RetainCycleExample {
    var id = 10
    // The closure property
    var closure: (() -> Void)?
    func setupClosureBug() {
        // Capturing self strongly inside the closure
        closure = {
            self.id = 20
        }
    }

     func setupClosureOk() {
       closure = { [weak self] in
            self?.id = 20
        }
    }
}
