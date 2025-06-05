use std::io::Write;

use stable_mir::CrateItems;

// TODO TEXTUAL Definitions & Translations here!

pub fn mir_to_textual(items: CrateItems, out: &mut impl Write) {
    for item in items {
        // TODO Translate MIR to textual
        write!(out, "Item: {:?}", item).expect("Item");
        write!(out, "{:?}", item.body().expect("No Body").blocks).expect("Body");
    }
}
