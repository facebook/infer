// Minimal reproducible test case for indirect closure retain cycle

class InnerWrapper {
    let actionClosure: () -> Void

    init(actionClosure: @escaping () -> Void) {
        self.actionClosure = actionClosure
    }
}

class MainController {
    private var wrapper: InnerWrapper?
    public var configValue: Int = 0

    public func setup() {
        // Line 17: Allocation of the inner object and passing the closure
        let newWrapper = InnerWrapper(
            actionClosure: createClosure()
        )

        // Line 22: Assignment to self, closing the cycle
        wrapper = newWrapper
    }

    private func createClosure() -> () -> Void {
        return createClosure(configValue: configValue)
    }

    private func createClosure(configValue: Int) -> () -> Void {
        // Intentionally strong capture of `self` to trigger the cycle
        return {
            _ = self.configValue
        }
    }
}
