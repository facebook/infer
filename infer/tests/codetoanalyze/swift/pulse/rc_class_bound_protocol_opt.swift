import Foundation

public class OptMinimalBaseView: NSObject {}

protocol OptMinimalContentViewType: OptMinimalBaseView {
    var delegate: AnyObject? { get set }
}

class OptDummyMinimalView: OptMinimalBaseView, OptMinimalContentViewType {
    var delegate: AnyObject?
}

final class OptMinimalCell {
    var contentView: OptMinimalContentViewType?

    init() {
        contentView = OptDummyMinimalView()
    }

    func configure_opt_bad_fn() {
        contentView?.delegate = self
    }
}

func test_opt_minimal_retain_cycle_bad_fn() {
    let cell = OptMinimalCell()
    cell.configure_opt_bad_fn()
}
