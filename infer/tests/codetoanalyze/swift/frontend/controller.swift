class ViewController {
    var view: CustomView?

    init() {
        self.view = CustomView(delegate: self)
    }
}

class CustomView {
    var delegate: ViewController

    init(delegate: ViewController) {
        self.delegate = delegate
    }
}

func retainCycleExample() {
    let _ = ViewController()
}
