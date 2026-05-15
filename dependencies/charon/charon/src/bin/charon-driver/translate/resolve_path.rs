//! Machinery to resolve a string path into a `DefId`. Based on `clippy_utils::def_path_res`.
use std::sync::Arc;

use anyhow::bail;
use charon_lib::name_matcher::NamePattern;
use hax::{BaseState, SInto};
use itertools::Itertools;
use rustc_ast::Mutability;
use rustc_hir::def_id::{CrateNum, DefId, LOCAL_CRATE};
use rustc_middle::ty::{self, FloatTy, IntTy, TyCtxt, UintTy, fast_reject::SimplifiedType};
use rustc_span::symbol::Symbol;

fn find_primitive_impls<'tcx>(
    tcx: TyCtxt<'tcx>,
    name: &str,
) -> impl Iterator<Item = DefId> + use<'tcx> {
    let ty = match name {
        "bool" => SimplifiedType::Bool,
        "char" => SimplifiedType::Char,
        "str" => SimplifiedType::Str,
        "array" => SimplifiedType::Array,
        "slice" => SimplifiedType::Slice,
        // FIXME: rustdoc documents these two using just `pointer`.
        //
        // Maybe this is something we should do here too.
        "const_ptr" => SimplifiedType::Ptr(Mutability::Not),
        "mut_ptr" => SimplifiedType::Ptr(Mutability::Mut),
        "isize" => SimplifiedType::Int(IntTy::Isize),
        "i8" => SimplifiedType::Int(IntTy::I8),
        "i16" => SimplifiedType::Int(IntTy::I16),
        "i32" => SimplifiedType::Int(IntTy::I32),
        "i64" => SimplifiedType::Int(IntTy::I64),
        "i128" => SimplifiedType::Int(IntTy::I128),
        "usize" => SimplifiedType::Uint(UintTy::Usize),
        "u8" => SimplifiedType::Uint(UintTy::U8),
        "u16" => SimplifiedType::Uint(UintTy::U16),
        "u32" => SimplifiedType::Uint(UintTy::U32),
        "u64" => SimplifiedType::Uint(UintTy::U64),
        "u128" => SimplifiedType::Uint(UintTy::U128),
        "f32" => SimplifiedType::Float(FloatTy::F32),
        "f64" => SimplifiedType::Float(FloatTy::F64),
        _ => {
            return [].iter().copied();
        }
    };
    tcx.incoherent_impls(ty).iter().copied()
}

/// Find the items corresponding to a given pattern. This mostly ignores generics.
///
/// Can return multiple resolutions when there are multiple versions of the same crate, e.g.
/// `memchr::memchr` could return the functions from both memchr 1.0 and memchr 2.0.
///
/// Also returns multiple results when there are multiple paths under the same name e.g. `std::vec`
/// would have both a [`hax::DefKind::Mod`] and [`hax::DefKind::Macro`].
///
/// This function is expensive and should be used sparingly.
///
/// If the path does not correspond to an existing item, return the first subpath that doesn't
/// correspond to an item.
pub fn def_path_def_ids<'a, 'tcx>(
    s: &impl BaseState<'tcx>,
    pat: &'a NamePattern,
    strict: bool,
) -> anyhow::Result<Vec<DefId>> {
    use charon_lib::name_matcher::{PatElem, PatTy};
    let tcx = s.base().tcx;
    let mut items: Vec<DefId> = vec![];
    for (i, elem) in pat.elems.iter().enumerate() {
        if i == 0 {
            match elem {
                PatElem::Ident { name: elem, .. } => {
                    let segment = Symbol::intern(elem);
                    items = tcx
                        .crates(())
                        .iter()
                        .copied()
                        .chain([LOCAL_CRATE])
                        // Also consider "crate" to be a valid name for the local crate.
                        .filter(move |&num| {
                            tcx.crate_name(num) == segment
                                || (num == LOCAL_CRATE && elem == "crate")
                        })
                        .map(CrateNum::as_def_id)
                        .collect_vec();
                    items.extend(find_primitive_impls(tcx, elem));
                }
                PatElem::Glob => {
                    items = tcx
                        .crates(())
                        .iter()
                        .copied()
                        .chain([LOCAL_CRATE])
                        .map(CrateNum::as_def_id)
                        .collect_vec();
                }
                PatElem::Impl(impl_pat) => match impl_pat.elems.as_slice() {
                    [
                        ..,
                        PatElem::Ident {
                            generics,
                            is_trait: true,
                            ..
                        },
                    ] => match generics.as_slice() {
                        [] => bail!("malformed trait impl pattern"),
                        [PatTy::Pat(self_pat)] => {
                            let impls = def_path_def_ids(s, impl_pat, strict)?
                                .into_iter()
                                .flat_map(|trait_def_id| tcx.all_impls(trait_def_id));
                            match self_pat.elems.as_slice() {
                                [PatElem::Glob] => {
                                    items = impls.collect_vec();
                                }
                                _ => {
                                    let self_ty_def_ids = def_path_def_ids(s, self_pat, strict)?;
                                    items = impls
                                        .filter(|impl_def_id| {
                                            let impl_self_ty = tcx
                                                .impl_trait_ref(impl_def_id)
                                                .skip_binder()
                                                .self_ty();
                                            if let ty::Adt(adt_def, _) = impl_self_ty.kind() {
                                                self_ty_def_ids.contains(&adt_def.did())
                                            } else {
                                                false
                                            }
                                        })
                                        .collect_vec();
                                }
                            }
                        }
                        [_] => bail!("`--start-from` only supports implementations on named types"),
                        [_, _, ..] => bail!("`--start-from` does not support trait generics"),
                    },
                    [
                        ..,
                        PatElem::Ident {
                            is_trait: false, ..
                        },
                    ] => bail!("`--start-from` does not support inherent impls"),
                    _ => bail!("`--start-from` does not support this impl pattern"),
                },
            }
        } else {
            let nameable_children = items.iter().copied().flat_map(|def_id| {
                let hax_def: Arc<hax::FullDef> = def_id.sinto(s).full_def(s);
                hax_def.nameable_children(s)
            });
            match elem {
                PatElem::Ident { name: elem, .. } => {
                    items = nameable_children
                        .filter(|(child_name, _)| child_name.as_str() == elem)
                        .filter_map(|(_, def_id)| def_id.as_rust_def_id())
                        .collect();
                }
                PatElem::Glob => {
                    items = nameable_children
                        .filter_map(|(_, def_id)| def_id.as_rust_def_id())
                        .collect();
                }
                PatElem::Impl(_) => bail!(
                    "`--start-from` only supports impl patterns if they're the first element of the path"
                ),
            }
        }
        if strict && items.is_empty() {
            let prefix = NamePattern {
                elems: pat.elems[..=i].to_vec(),
            };
            if i == 0 {
                bail!(
                    "path `{prefix}` does not correspond to any item; did you mean `crate::{prefix}`?"
                )
            } else {
                bail!("path `{prefix}` does not correspond to any item")
            }
        }
    }
    Ok(items)
}
