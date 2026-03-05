import UIKit

class WidgetDisplayView: UIView {
    var clickCount = 0
    var hostCell: DashboardWidgetCell?

    // FOCUS: Pulse MUST devirtualize to jump in here and see the assignment.
    // (Currently a FALSE NEGATIVE: devirtualization works, but the setter modeling drops the assignment)
    func setup(with cell: DashboardWidgetCell) {
        self.hostCell = cell
    }
}

struct WidgetBuilder {
    @MainActor static func createWidgetView() -> WidgetDisplayView {
        return WidgetDisplayView()
    }
}

final class DashboardWidgetCell: UICollectionViewCell {
    var embeddedWidget: WidgetDisplayView?

    func configureCell() {
        let newWidget = WidgetBuilder.createWidgetView()

        // 1. Cell strongly retains the view
        self.embeddedWidget = newWidget

        // 2. We pass `self` into the virtual call.
        newWidget.setup(with: self)

        newWidget.clickCount = 1
    }
}
