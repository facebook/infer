//! Machinery to resolve a string path into a `DefId`. Based on `clippy_utils::def_path_res`.
use std::sync::Arc;

use hax_frontend_exporter::{self as hax, BaseState, SInto};
use itertools::Itertools;
use rustc_ast::Mutability;
use rustc_hir::def_id::{CrateNum, DefId, LOCAL_CRATE};
use rustc_middle::ty::{FloatTy, IntTy, TyCtxt, UintTy, fast_reject::SimplifiedType};
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

/// Resolves a def path like `std::vec::Vec`.
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
    path: &'a [&'a str],
) -> Result<Vec<DefId>, &'a [&'a str]> {
    let tcx = s.base().tcx;
    let mut items = vec![];
    for (i, &segment_str) in path.iter().enumerate() {
        if i == 0 {
            let segment = Symbol::intern(segment_str);
            items = tcx
                .crates(())
                .iter()
                .copied()
                .chain([LOCAL_CRATE])
                .filter(move |&num| tcx.crate_name(num) == segment)
                // Also consider "crate" a valid name for the local crate.
                .chain(if segment_str == "crate" {
                    Some(LOCAL_CRATE)
                } else {
                    None
                })
                .map(CrateNum::as_def_id)
                .collect_vec();
            items.extend(find_primitive_impls(tcx, segment_str));
        } else {
            items = items
                .into_iter()
                .flat_map(|def_id| {
                    let hax_def: Arc<hax::FullDef> = def_id.sinto(s).full_def(s);
                    hax_def.nameable_children(s)
                })
                .filter(|(child_name, _)| *child_name == segment_str)
                .filter_map(|(_, def_id)| def_id.as_rust_def_id())
                .collect();
        }
        if items.is_empty() {
            return Err(&path[..=i]);
        }
    }
    Ok(items)
}
