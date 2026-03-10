import Foundation

public class MinimalBaseView: NSObject {}

protocol MinimalContentViewType: MinimalBaseView {
    var delegate: AnyObject? { get set }
}

class DummyMinimalView: MinimalBaseView, MinimalContentViewType {
    var delegate: AnyObject?
}

final class MinimalCell {
    var contentView: MinimalContentViewType

    init() {
        contentView = DummyMinimalView()
    }

    func configure_bad() {
        contentView.delegate = self
    }
}

func test_minimal_retain_cycle_bad() {
    let cell = MinimalCell()
    cell.contentView = DummyMinimalView()
    cell.configure_bad()
}
