struct Quality: Equatable {
    let value: Int
    let sndValue: Int
}

func retain_cycle_variable_shadow_fp(quality: Int) -> Quality {
    let quality = Quality(value: quality, sndValue: 0)
    return quality
}
