//@ charon-args=--polonius
#![allow(dead_code)]
use std::collections::HashMap;

/// The example the Rust team uses to illustrate why we need Polonius.
pub fn get_or_insert(map: &mut HashMap<u32, u32>) -> &u32 {
    match map.get(&22) {
        Some(v) => v,
        None => {
            map.insert(22, 33);
            &map[&22]
        }
    }
}
