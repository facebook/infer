import Foundation


struct Theme {
    var isDark: Bool = false
}

class UserSettings {
    var theme = Theme()
}

func modify_test() {
  let settings = UserSettings()
  // This line triggers the .modify accessor
  settings.theme.isDark = true
}
