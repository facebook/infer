//! This generates a rare case of a `DefPathData::ValueNs` with non-zero disambiguator.
fn nonzero_disambiguator() {
    if true {
        fn my_function() {}
        my_function()
    } else {
        fn my_function() {}
        my_function()
    }
}
