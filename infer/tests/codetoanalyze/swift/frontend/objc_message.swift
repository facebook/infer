import UIKit

class DummyContentView: UIView {
    func configure() {}
}

struct BCNViewUtils {
    @MainActor static func contentViewForVariant() -> DummyContentView {
        return DummyContentView()
    }
}

final class BCNCell: UICollectionViewCell {
    var nuxContentView: DummyContentView?

    func configure() {
        // FOCUS: This call generates the objc_msgSend we want to resolve
        let newView = BCNViewUtils.contentViewForVariant()
        self.nuxContentView = newView

        // FOCUS: This is the resolved virtual call
        newView.configure()
    }
}
