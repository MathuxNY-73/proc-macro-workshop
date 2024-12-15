use std::any;

// Crates that have the "proc-macro" crate type are only allowed to export
// procedural macros. So we cannot have one crate that defines procedural macros
// alongside other types of public APIs like traits and structs.
//
// For this project we are going to need a #[bitfield] macro but also a trait
// and some structs. We solve this by defining the trait and structs in this
// crate, defining the attribute macro in a separate bitfield-impl crate, and
// then re-exporting the macro from this crate so that users only have one crate
// that they need to import.
//
// From the perspective of a user of this crate, they get all the necessary APIs
// (macro, trait, struct) through the one bitfield crate.
pub use bitfield_impl::{bitfield, gen};

use thiserror::Error;
use anyhow::{self, Context};
use anyhow::bail;

pub trait Specifier {
  const BITS: usize;

  type T: Sized;

  fn set<const ACC: usize, const SIZE: usize>(arr: &mut [u8], num: <Self as Specifier>::T);
  fn get<const ACC: usize, const SIZE: usize>(arr: &[u8]) -> <Self as Specifier>::T;
}

gen! {}

#[derive(Error, Debug)]
#[error("Value {0} overflows the number of bits {BITS}")]
struct OverflowError<const BITS: usize, T>(T);

struct BitsU8<const BITS: usize, const START: usize, const SIZE: usize>;

impl<const BITS: usize, const START: usize, const SIZE: usize> BitsU8<BITS, START, SIZE> {
  const START_IDX: usize = START / u8::BITS as usize;
  const END_IDX: usize = (START + BITS -  1) / u8::BITS as usize;
  const OFFSET: usize = START % u8::BITS as usize;
  const IS_ACROSS: bool = Self::OFFSET + BITS > u8::BITS as usize;

  const CHECK_: () = [()][(Self::END_IDX >= SIZE ||
                           BITS > u8::BITS as usize) as usize];
  const MASK_LHS: u8 =
    if Self::OFFSET == 0 { 0 }
    else { u8::MAX << (u8::BITS as usize - Self::OFFSET) };
  const MASK_RHS: u8 =
    if Self::OFFSET == 0 && BITS == u8::BITS as usize { 0 }
    else {
      u8::MAX >> (BITS + Self::OFFSET
                  - u8::BITS as usize * [0, 1][Self::IS_ACROSS as usize])};

  const fn does_fit(val: u8) -> bool {
    let val_num_bits = (u8::BITS - val.leading_zeros()) as usize;
    val_num_bits <= BITS
  }

  fn set_bits(data: &mut [u8], val: u8) -> anyhow::Result<()> {
    let _ = Self::CHECK_;
    if !Self::does_fit(val) {
      bail!(OverflowError::<BITS, u8>(val));
    }

    let slice = &mut data[Self::START_IDX..=Self::START_IDX];
    let old_val = u8::from_ne_bytes(slice.try_into().unwrap());
    let new_val = old_val & (Self::MASK_LHS ^ Self::MASK_RHS) | val << (u8::BITS as usize - BITS - Self::OFFSET);
    slice.copy_from_slice(&new_val.to_ne_bytes());
    Ok(())
  }

  fn get_bits(data: &[u8]) -> u8 {
    let _ = Self::CHECK_;

    let slice = &data[Self::START_IDX..=Self::START_IDX];
    let val = u8::from_ne_bytes(slice.try_into().unwrap());
    (val & !(Self::MASK_LHS ^ Self::MASK_RHS)) >> (u8::BITS as usize - BITS - Self::OFFSET)
  }

  fn set_bits_accross(
      data: &mut [u8], val: u8) -> anyhow::Result<()> {
    let _ = Self::CHECK_;

    if !Self::does_fit(val) {
      bail!(OverflowError::<BITS, u8>(val));
    }

    let lhs_slice = &mut data[Self::START_IDX..=Self::START_IDX];
    let lhs = u8::from_ne_bytes(lhs_slice.try_into().unwrap());
    let new_lhs = lhs & Self::MASK_LHS | val >> (BITS + Self::OFFSET - u8::BITS as usize);
    lhs_slice.copy_from_slice(&new_lhs.to_ne_bytes());

    let rhs_slice = &mut data[Self::END_IDX..=Self::END_IDX];
    let rhs = u8::from_ne_bytes(rhs_slice.try_into().unwrap());
    let new_rhs = rhs & Self::MASK_RHS | val << (2 * u8::BITS as usize - BITS - Self::OFFSET);
    rhs_slice.copy_from_slice(&new_rhs.to_ne_bytes());

    Ok(())
  }

  fn get_bits_accross(data: &[u8]) -> u8 {
    let _ = Self::CHECK_;

    let lhs_slice = &data[Self::START_IDX..=Self::START_IDX];
    let lhs = u8::from_ne_bytes(lhs_slice.try_into().unwrap());
    let lhs = (lhs & !Self::MASK_LHS) << (BITS + Self::OFFSET - u8::BITS as usize);

    let rhs_slice = &data[Self::END_IDX..=Self::END_IDX];
    let rhs = u8::from_ne_bytes(rhs_slice.try_into().unwrap());
    let rhs = (rhs & !Self::MASK_RHS) >> (2 * u8::BITS as usize - BITS - Self::OFFSET);

    lhs | rhs
  }

}



#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_get_bits() {
    let data = [
      0b10101010,
      0b11110000,
      0b00110011,
      0b11001100,
      0b00111100];

    // Inside u8
    assert_eq!(BitsU8::<3, 20, 5>::END_IDX, 2);
    assert_eq!(BitsU8::<3, 20, 5>::START_IDX, 2);
    assert_eq!(BitsU8::<3, 20, 5>::OFFSET, 4);
    assert_eq!(BitsU8::<3, 20, 5>::MASK_LHS, 0b11110000);
    assert_eq!(BitsU8::<3, 20, 5>::MASK_RHS, 0b00000001);
    assert_eq!(BitsU8::<3, 20, 5>::get_bits(&data), 0b00000001);

    assert_eq!(BitsU8::<8, 32, 5>::END_IDX, 4);
    assert_eq!(BitsU8::<8, 32, 5>::START_IDX, 4);
    assert_eq!(BitsU8::<8, 32, 5>::OFFSET, 0);
    assert_eq!(BitsU8::<8, 32, 5>::MASK_LHS, 0b0000000);
    assert_eq!(BitsU8::<8, 32, 5>::MASK_RHS, 0b0000000);
    assert_eq!(BitsU8::<8, 32, 5>::get_bits(&data), 0b00111100);

    // Accross u8
    assert_eq!(BitsU8::<8, 20, 5>::END_IDX, 3);
    assert_eq!(BitsU8::<8, 20, 5>::START_IDX, 2);
    assert_eq!(BitsU8::<8, 20, 5>::OFFSET, 4);
    assert_eq!(BitsU8::<8, 20, 5>::MASK_LHS, 0b11110000);
    assert_eq!(BitsU8::<8, 20, 5>::MASK_RHS, 0b00001111);
    assert_eq!(BitsU8::<8, 20, 5>::get_bits_accross(&data), 0b00111100);

    assert_eq!(BitsU8::<2, 7, 5>::END_IDX, 1);
    assert_eq!(BitsU8::<2, 7, 5>::START_IDX, 0);
    assert_eq!(BitsU8::<2, 7, 5>::OFFSET, 7);
    assert_eq!(BitsU8::<2, 7, 5>::MASK_LHS, 0b11111110);
    assert_eq!(BitsU8::<2, 7, 5>::MASK_RHS, 0b01111111);
    assert_eq!(BitsU8::<2, 7, 5>::get_bits_accross(&data), 0b00000001);
  }

  #[test]
  fn test_set_bits() {
    let mut data = [
      0b10101010,
      0b11110000,
      0b00110011,
      0b11001100,
      0b00111100];

    // Inside u8
    assert_eq!(BitsU8::<4, 16, 5>::get_bits(&data), 0b00000011);
    assert!(BitsU8::<4, 16, 5>::set_bits(&mut data, 0b00001010).is_ok());
    assert_eq!(BitsU8::<4, 16, 5>::get_bits(&data), 0b00001010);

    assert_eq!(BitsU8::<8, 0, 5>::get_bits(&data), 0b10101010);
    assert!(BitsU8::<8, 0, 5>::set_bits(&mut data, 0b11100011).is_ok());
    assert_eq!(BitsU8::<8, 0, 5>::get_bits(&data), 0b11100011);

    // Accross u8
    assert_eq!(BitsU8::<8, 20, 5>::get_bits_accross(&data), 0b00111100);
    assert!(BitsU8::<8, 20, 5>::set_bits_accross(&mut data, 0b01001001).is_ok());
    assert_eq!(BitsU8::<8, 20, 5>::get_bits_accross(&data), 0b01001001);

    assert_eq!(BitsU8::<2, 7, 5>::get_bits_accross(&data), 0b00000011);
    assert!(BitsU8::<2, 7, 5>::set_bits_accross(&mut data, 0b00000001).is_ok());
    assert_eq!(BitsU8::<2, 7, 5>::get_bits_accross(&data), 0b00000001);

    assert_eq!(data, [
      0b11100010,
      0b11110000,
      0b10100100,
      0b10011100,
      0b00111100
    ]);
  }
}
