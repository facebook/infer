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
