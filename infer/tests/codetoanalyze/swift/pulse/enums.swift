
enum Constants {
    static let defaultPadding: Int = 16
    static let maxItems: Int = 5
}

func using_enums_with_defaultPadding_bad() {
    let padding = Constants.defaultPadding
    assert(padding == 10) //assertion error reported here because padding is 16
}

func using_enums_with_defaultPadding_good() {
    let padding = Constants.defaultPadding
    assert(padding == 16) //assertion error not reported here because padding is 16
}

func using_enums_with_defaultPadding() -> Int {
    let padding = Constants.defaultPadding
    return padding
}
