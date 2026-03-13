final class State {
    var delegate: DeviceAppManagerClientDelegate?
}

protocol DeviceAppManagerClientDelegate: AnyObject {}

final class DeviceAppManagerDelegateImpl: DeviceAppManagerClientDelegate {
    let onStartServiceResponse: () -> Void
    init(onStartServiceResponse: @escaping () -> Void) {
        self.onStartServiceResponse = onStartServiceResponse
    }
}

func foo(_ state : State?) {}

func test_retain_cycle_bad2() {
    // Set up state and a reference to it
    let state = State()
    var stateRef: State? = state

    // Create the delegate, capturing stateRef in the closure
    let delegate = DeviceAppManagerDelegateImpl {
        // This closure captures stateRef, creating a retain cycle
        stateRef = nil
    }
    foo(stateRef)

    state.delegate = delegate
}
