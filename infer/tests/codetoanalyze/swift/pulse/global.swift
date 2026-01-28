import UIKit
// Define the spacing constant
let kIGDSSpacingStandard: CGFloat = 8.0
// Function to return the spacing value
func getSpacing() -> CGFloat {
    return kIGDSSpacingStandard
}
// Your custom cell class
final class BCNTypeaheadSuggestionsCell: UICollectionViewCell {
    // Static property using getSpacing()
    private static let contentInsets = UIEdgeInsets(
        top: getSpacing(),
        left: getSpacing(),
        bottom: getSpacing(),
        right: getSpacing()
    )
}
