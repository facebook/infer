use std::collections::{HashMap, hash_map::Entry};

use crate::ast::*;

use super::{TransformCtx, ctx::TransformPass};

enum FoundName<'a> {
    Unique {
        long: &'a [PathElem],
        ids: Vec<AnyTransId>,
    },
    Multiple,
}

pub struct Transform;
impl TransformPass for Transform {
    fn transform_ctx(&self, ctx: &mut TransformCtx) {
        let mut short_names: HashMap<&str, FoundName> = Default::default();
        for (&id, name) in &ctx.translated.item_names {
            let mut name_slice = name.name.as_slice();

            // Trait impls are sufficiently unique information, so truncate starting from the
            // rightmost impl.
            if let Some((i, _)) = name_slice
                .iter()
                .enumerate()
                .rfind(|(_, elem)| matches!(elem, PathElem::Impl(ImplElem::Trait(..), ..)))
            {
                name_slice = &name.name[i..];
                let trunc_name = Name {
                    name: name_slice.to_vec(),
                };
                ctx.translated.short_names.insert(id, trunc_name);
            }

            if let [prefix @ .., PathElem::Monomorphized(..)] = name_slice {
                name_slice = prefix;
            }
            // Ignoring monomorphizations, if a name is the only one to end with a given suffix, we
            // accumulate the ids of all the items with that name (there may be several thanks to
            // monomorphizations).
            match name_slice {
                [.., PathElem::Ident(ident, _)] => match short_names.entry(ident) {
                    Entry::Occupied(mut e) => match e.get_mut() {
                        FoundName::Unique { long, ids } => {
                            if *long == name_slice {
                                ids.push(id)
                            } else {
                                e.insert(FoundName::Multiple);
                            }
                        }
                        FoundName::Multiple => {}
                    },
                    Entry::Vacant(e) => {
                        e.insert(FoundName::Unique {
                            long: name_slice,
                            ids: vec![id],
                        });
                    }
                },
                _ => {}
            }
        }

        for (short, found) in short_names {
            if let FoundName::Unique { ids, .. } = found {
                for id in ids {
                    let mut short_name = Name {
                        name: vec![PathElem::Ident(short.to_owned(), Disambiguator::ZERO)],
                    };
                    if let [.., mono @ PathElem::Monomorphized(..)] =
                        ctx.translated.item_names[&id].name.as_slice()
                    {
                        short_name.name.push(mono.clone());
                    }
                    ctx.translated.short_names.insert(id, short_name);
                }
            }
        }
    }
}
