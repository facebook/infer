//! Implementations for [crate::values]
use crate::ast::*;

#[derive(Debug, Clone)]
pub enum ScalarError {
    /// Attempt to use a signed scalar as an unsigned scalar or vice-versa
    IncorrectSign,
    /// Out of bounds scalar
    OutOfBounds,
    UnsupportedPtrSize,
}
/// Our redefinition of Result - we don't care much about the I/O part.
pub type ScalarResult<T> = std::result::Result<T, ScalarError>;

macro_rules! from_le_bytes {
    ($m:ident, $b:ident, [$(($i_ty: ty, $i:ident, $s:ident, $n_ty:ty, $t:ty)),*]) => {
        match $m {
            $(
                IntegerTy::$s(<$i_ty>::$i) => {
                    let n = size_of::<$n_ty>();
                    let b: [u8; _] = $b[0..n].try_into().unwrap();
                    ScalarValue::$s(<$i_ty>::$i, <$n_ty>::from_le_bytes(b) as $t)
                }
            )*
        }
    }
}

impl ScalarValue {
    fn ptr_size_max(ptr_size: ByteCount, signed: bool) -> ScalarResult<u128> {
        match ptr_size {
            2 => Ok(if signed {
                i16::MAX as u128
            } else {
                u16::MAX as u128
            }),
            4 => Ok(if signed {
                i32::MAX as u128
            } else {
                u32::MAX as u128
            }),
            8 => Ok(if signed {
                i64::MAX as u128
            } else {
                u64::MAX as u128
            }),
            _ => Err(ScalarError::UnsupportedPtrSize),
        }
    }

    fn ptr_size_min(ptr_size: ByteCount, signed: bool) -> ScalarResult<i128> {
        match ptr_size {
            2 => Ok(if signed {
                i16::MIN as i128
            } else {
                u16::MIN as i128
            }),
            4 => Ok(if signed {
                i32::MIN as i128
            } else {
                u32::MIN as i128
            }),
            8 => Ok(if signed {
                i64::MIN as i128
            } else {
                u64::MIN as i128
            }),
            _ => Err(ScalarError::UnsupportedPtrSize),
        }
    }

    pub fn get_integer_ty(&self) -> IntegerTy {
        match self {
            ScalarValue::Signed(ty, _) => IntegerTy::Signed(*ty),
            ScalarValue::Unsigned(ty, _) => IntegerTy::Unsigned(*ty),
        }
    }

    pub fn is_int(&self) -> bool {
        matches!(self, ScalarValue::Signed(_, _))
    }

    pub fn is_uint(&self) -> bool {
        matches!(self, ScalarValue::Unsigned(_, _))
    }

    /// When computing the result of binary operations, we convert the values
    /// to u128 then back to the target type (while performing dynamic checks
    /// of course).
    pub fn as_uint(&self) -> ScalarResult<u128> {
        match self {
            ScalarValue::Unsigned(_, v) => Ok(*v),
            _ => Err(ScalarError::IncorrectSign),
        }
    }

    pub fn uint_is_in_bounds(ptr_size: ByteCount, ty: UIntTy, v: u128) -> bool {
        match ty {
            UIntTy::Usize => v <= Self::ptr_size_max(ptr_size, false).unwrap(),
            UIntTy::U8 => v <= (u8::MAX as u128),
            UIntTy::U16 => v <= (u16::MAX as u128),
            UIntTy::U32 => v <= (u32::MAX as u128),
            UIntTy::U64 => v <= (u64::MAX as u128),
            UIntTy::U128 => true,
        }
    }

    pub fn from_unchecked_uint(ty: UIntTy, v: u128) -> ScalarValue {
        ScalarValue::Unsigned(ty, v)
    }

    pub fn from_uint(ptr_size: ByteCount, ty: UIntTy, v: u128) -> ScalarResult<ScalarValue> {
        if !ScalarValue::uint_is_in_bounds(ptr_size, ty, v) {
            trace!("Not in bounds for {:?}: {}", ty, v);
            Err(ScalarError::OutOfBounds)
        } else {
            Ok(ScalarValue::from_unchecked_uint(ty, v))
        }
    }

    /// When computing the result of binary operations, we convert the values
    /// to i128 then back to the target type (while performing dynamic checks
    /// of course).
    pub fn as_int(&self) -> ScalarResult<i128> {
        match self {
            ScalarValue::Signed(_, v) => Ok(*v),
            _ => Err(ScalarError::IncorrectSign),
        }
    }

    pub fn int_is_in_bounds(ptr_size: ByteCount, ty: IntTy, v: i128) -> bool {
        match ty {
            IntTy::Isize => {
                v >= Self::ptr_size_min(ptr_size, true).unwrap()
                    && v <= Self::ptr_size_max(ptr_size, true).unwrap() as i128
            }
            IntTy::I8 => v >= (i8::MIN as i128) && v <= (i8::MAX as i128),
            IntTy::I16 => v >= (i16::MIN as i128) && v <= (i16::MAX as i128),
            IntTy::I32 => v >= (i32::MIN as i128) && v <= (i32::MAX as i128),
            IntTy::I64 => v >= (i64::MIN as i128) && v <= (i64::MAX as i128),
            IntTy::I128 => true,
        }
    }

    pub fn from_unchecked_int(ty: IntTy, v: i128) -> ScalarValue {
        ScalarValue::Signed(ty, v)
    }

    /// Most integers are represented as `u128` by rustc. We must be careful not to sign-extend.
    pub fn to_bits(&self) -> u128 {
        match *self {
            ScalarValue::Unsigned(_, v) => v,
            ScalarValue::Signed(_, v) => u128::from_le_bytes(v.to_le_bytes()),
        }
    }

    /// Translates little endian bytes into a corresponding `ScalarValue`.
    /// This needs to do the round-trip to the correct integer type to guarantee
    /// that the values are correctly sign-extended (e.g. if the bytes encode -1i8, taking all 16 bytes
    /// would lead to the value 255i128 instead of -1i128).
    pub fn from_le_bytes(ty: IntegerTy, bytes: [u8; 16]) -> Self {
        from_le_bytes!(
            ty,
            bytes,
            [
                (IntTy, Isize, Signed, isize, i128),
                (IntTy, I8, Signed, i8, i128),
                (IntTy, I16, Signed, i16, i128),
                (IntTy, I32, Signed, i32, i128),
                (IntTy, I64, Signed, i64, i128),
                (IntTy, I128, Signed, i128, i128),
                (UIntTy, Usize, Unsigned, usize, u128),
                (UIntTy, U8, Unsigned, u8, u128),
                (UIntTy, U16, Unsigned, u16, u128),
                (UIntTy, U32, Unsigned, u32, u128),
                (UIntTy, U64, Unsigned, u64, u128),
                (UIntTy, U128, Unsigned, u128, u128)
            ]
        )
    }

    pub fn from_bits(ty: IntegerTy, bits: u128) -> Self {
        let bytes = bits.to_le_bytes();
        Self::from_le_bytes(ty, bytes)
    }

    /// **Warning**: most constants are stored as u128 by rustc. When converting
    /// to i128, it is not correct to do `v as i128`, we must reinterpret the
    /// bits (see [ScalarValue::from_le_bytes]).
    pub fn from_int(ptr_size: ByteCount, ty: IntTy, v: i128) -> ScalarResult<ScalarValue> {
        if !ScalarValue::int_is_in_bounds(ptr_size, ty, v) {
            Err(ScalarError::OutOfBounds)
        } else {
            Ok(ScalarValue::from_unchecked_int(ty, v))
        }
    }

    pub fn to_constant(self) -> ConstantExpr {
        let literal_ty = match self {
            ScalarValue::Signed(int_ty, _) => LiteralTy::Int(int_ty),
            ScalarValue::Unsigned(uint_ty, _) => LiteralTy::UInt(uint_ty),
        };
        ConstantExpr {
            value: RawConstantExpr::Literal(Literal::Scalar(self)),
            ty: TyKind::Literal(literal_ty).into_ty(),
        }
    }
}

/// Custom serializer that stores 128 bit integers as strings to avoid overflow.
pub(crate) mod scalar_value_ser_de {
    use std::{marker::PhantomData, str::FromStr};

    use serde::de::{Deserializer, Error};

    pub fn serialize<S, V>(val: &V, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::ser::Serializer,
        V: ToString,
    {
        serializer.serialize_str(&val.to_string())
    }

    pub fn deserialize<'de, D, V>(deserializer: D) -> Result<V, D::Error>
    where
        D: Deserializer<'de>,
        V: FromStr,
    {
        struct Visitor<V> {
            _val: PhantomData<V>,
        }
        impl<'de, V> serde::de::Visitor<'de> for Visitor<V>
        where
            V: FromStr,
        {
            type Value = V;
            fn expecting(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                write!(f, "ScalarValue value")
            }
            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
            where
                E: Error,
            {
                v.parse()
                    .map_err(|_| E::custom("Could not parse 128 bit integer!"))
            }
        }
        deserializer.deserialize_str(Visitor { _val: PhantomData })
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_big_endian_scalars() -> ScalarResult<()> {
        let u128 = 0x12345678901234567890123456789012u128;
        let le_bytes = u128.to_le_bytes();

        let le_scalar = ScalarValue::from_le_bytes(IntegerTy::Unsigned(UIntTy::U128), le_bytes);
        assert_eq!(le_scalar, ScalarValue::Unsigned(UIntTy::U128, u128));

        let i64 = 0x1234567890123456i64;
        let le_bytes = (i64 as i128).to_le_bytes();
        let le_scalar = ScalarValue::from_le_bytes(IntegerTy::Signed(IntTy::I64), le_bytes);
        assert_eq!(le_scalar, ScalarValue::Signed(IntTy::I64, i64 as i128));

        Ok(())
    }
}
