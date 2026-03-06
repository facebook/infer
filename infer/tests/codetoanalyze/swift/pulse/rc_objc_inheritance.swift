import UIKit

class WidgetDisplayView: UIView {
    var clickCount = 0
    var hostCell: DashboardWidgetCell?

    // FOCUS: Pulse MUST devirtualize to jump in here and see the assignment.
    // Furthermore, because WidgetDisplayView inherits from an ObjC class (UIView),
    // Swift compiles this property assignment using an opaque dynamic byte-offset
    // loaded from a Field Offset Vector (Wvd global). The frontend must intercept
    // this `getelementptr i8` math and bridge it to the Textual `hostCell` field
    // so Pulse can successfully close the strong reference loop.
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

    func configureCell_bad() {
        let newWidget = WidgetBuilder.createWidgetView()

        // 1. Cell strongly retains the view
        self.embeddedWidget = newWidget

        // 2. We pass `self` into the virtual call.
        newWidget.setup(with: self)

        newWidget.clickCount = 1
    }
}
