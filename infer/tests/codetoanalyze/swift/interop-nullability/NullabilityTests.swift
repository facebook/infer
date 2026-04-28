import Foundation

func unannotatedReturn_bad(api: LegacyAPI) {
  let s = api.getUnannotatedString()!
  print(s.count)
}

func nonnullReturn_good(api: LegacyAPI) {
  let s = api.getNonnullString()
  print(s.count)
}

func nullableReturn_good(api: LegacyAPI) {
  if let s = api.getNullableString() {
    print(s.count)
  }
}

// FP today: the [nullable] qualifier is hidden behind a MacroQualifiedType
// wrapper on the property's type. Will become _good in the follow-up diff.
func macroAnnotatedNullableProp_good_FP(api: LegacyAPI) {
  if let s = api.macroAnnotatedNullableProp {
    print(s.count)
  }
}

func macroAnnotatedUnannotatedProp_bad(api: LegacyAPI) {
  let s = api.macroAnnotatedUnannotatedProp!
  print(s.count)
}
