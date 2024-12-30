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
use anyhow::{self, bail};

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

trait SetGet<const BITS: usize, const START: usize, const SIZE: usize> {
  type Target;

  const START_IDX: usize = START / u8::BITS as usize;
  const END_IDX: usize = (START + BITS -  1) / u8::BITS as usize;
  const OFFSET: usize = START % u8::BITS as usize;
  const IS_ACROSS: bool;

  const GET: fn(&[u8]) -> Self::Target;
  const SET: fn(&mut [u8], Self::Target) -> anyhow::Result<()>;

  const MASK_LHS: Self::Target;
  const MASK_RHS: u8 =
    if Self::OFFSET == 0 && BITS % 8 == 0 { 0 }
    else {
      u8::MAX >> ((BITS + Self::OFFSET) % 8)
    };
}

trait CheckInBoundary<const BITS: usize, const START: usize, const SIZE: usize>: SetGet<BITS, START, SIZE> {
  const CHECK: ();
}

impl<
  const BITS: usize,
  const START: usize,
  const SIZE: usize,
  T: SetGet<BITS, START, SIZE> + ?Sized> CheckInBoundary<BITS, START, SIZE> for T {
    const CHECK: () = [()][(Self::END_IDX >= SIZE) as usize];
}

struct BitsU8<const BITS: usize, const START: usize, const SIZE: usize>;

impl<const BITS: usize, const START: usize, const SIZE: usize> SetGet<BITS, START, SIZE> for BitsU8<BITS, START, SIZE> {
  type Target = u8;

  const MASK_LHS: u8 = if Self::OFFSET == 0 { 0 }
    else { u8::MAX << (u8::BITS as usize - Self::OFFSET) };
  const IS_ACROSS: bool = Self::OFFSET + BITS > u8::BITS as usize;
  const GET: fn(&[u8]) -> Self::Target = if Self::IS_ACROSS {
    Self::get_bits_accross
  } else {
    Self::get_bits
  };
  const SET: fn(&mut [u8], Self::Target) -> anyhow::Result<()> = if Self::IS_ACROSS {
    Self::set_bits_accross
  } else {
    Self::set_bits
  };
}

impl<const BITS: usize, const START: usize, const SIZE: usize> BitsU8<BITS, START, SIZE> {
  const CHECK_: () = [()][(BITS > u8::BITS as usize) as usize];

  const fn does_fit(val: u8) -> bool {
    let val_num_bits = (u8::BITS - val.leading_zeros()) as usize;
    val_num_bits <= BITS
  }

  fn set_bits(data: &mut [u8], val: u8) -> anyhow::Result<()> {
    let _ = Self::CHECK_;
    if !Self::does_fit(val) {
      bail!(OverflowError::<BITS, u8>(val));
    }

    let slice = &mut data[Self::START_IDX..=Self::END_IDX];
    let old_val = u8::from_ne_bytes(slice.try_into().unwrap());
    let new_val = old_val & (Self::MASK_LHS ^ Self::MASK_RHS) | val << (u8::BITS as usize - BITS - Self::OFFSET);
    slice.copy_from_slice(&new_val.to_ne_bytes());
    Ok(())
  }

  fn get_bits(data: &[u8]) -> u8 {
    let _ = Self::CHECK_;

    let slice = &data[Self::START_IDX..=Self::END_IDX];
    let val = u8::from_ne_bytes(slice.try_into().unwrap());
    (val & !(Self::MASK_LHS ^ Self::MASK_RHS)) >> (u8::BITS as usize - BITS - Self::OFFSET)
  }

  fn set_bits_accross(
      data: &mut [u8], val: u8) -> anyhow::Result<()> {
    let _ = Self::CHECK_;

    if !Self::does_fit(val) {
      bail!(OverflowError::<BITS, u8>(val));
    }

    let lhs_slice = &mut data[Self::START_IDX..Self::END_IDX];
    let lhs = u8::from_ne_bytes(lhs_slice.try_into().unwrap());
    let new_lhs = lhs & Self::MASK_LHS | val >> (BITS + Self::OFFSET - u8::BITS as usize);
    lhs_slice.copy_from_slice(&new_lhs.to_ne_bytes());

    let rhs_slice = &mut data[Self::END_IDX..=Self::END_IDX];
    let rhs = u8::from_ne_bytes(rhs_slice.try_into().unwrap());
    let new_rhs = rhs & Self::MASK_RHS | val << (u8::BITS as usize - ((BITS + Self::OFFSET) % 8));
    rhs_slice.copy_from_slice(&new_rhs.to_ne_bytes());

    Ok(())
  }

  fn get_bits_accross(data: &[u8]) -> u8 {
    let _ = Self::CHECK_;

    let lhs_slice = &data[Self::START_IDX..Self::END_IDX];
    let lhs = u8::from_ne_bytes(lhs_slice.try_into().unwrap());
    let lhs = (lhs & !Self::MASK_LHS) << (BITS + Self::OFFSET - u8::BITS as usize);

    let rhs_slice = &data[Self::END_IDX..=Self::END_IDX];
    let rhs = u8::from_ne_bytes(rhs_slice.try_into().unwrap());
    let rhs = (rhs & !Self::MASK_RHS) >> (u8::BITS as usize - ((BITS + Self::OFFSET) % 8));

    lhs | rhs
  }

}

struct BitsU16<const BITS: usize, const START: usize, const SIZE: usize>;

impl<const BITS: usize, const START: usize, const SIZE: usize> SetGet<BITS, START, SIZE> for BitsU16<BITS, START, SIZE> {
  type Target = u16;

  const MASK_LHS: Self::Target = if Self::OFFSET == 0 { 0 }
    else { u16::MAX << (u16::BITS as usize - Self::OFFSET) };

  const IS_ACROSS: bool = Self::OFFSET + BITS > u16::BITS as usize;
  const GET: fn(&[u8]) -> Self::Target = if Self::IS_ACROSS {
    Self::get_bits_accross
  } else {
    Self::get_bits
  };
  const SET: fn(&mut [u8], Self::Target) -> anyhow::Result<()> = if Self::IS_ACROSS {
    Self::set_bits_accross
  } else {
    Self::set_bits
  };
}

impl<const BITS: usize, const START: usize, const SIZE: usize> BitsU16<BITS, START, SIZE> {
  const CHECK_: () = [()][(BITS > u16::BITS as usize ||
                           BITS <= u8::BITS as usize) as usize];
  const READ: fn([u8; 2]) -> u16 = if cfg!(target_endian = "big") {
    u16::from_le_bytes
  } else {
    u16::from_be_bytes
  };
  const WRITE: fn(u16) -> [u8; 2] = if cfg!(target_endian = "big") {
    u16::to_le_bytes
  } else {
    u16::to_be_bytes
  };

  const fn does_fit(val: u16) -> bool {
    let val_num_bits = (u16::BITS - val.leading_zeros()) as usize;
    val_num_bits <= BITS
  }

  fn set_bits(data: &mut [u8], val: u16) -> anyhow::Result<()> {
    let _ = Self::CHECK_;
    if !Self::does_fit(val) {
      bail!(OverflowError::<BITS, u16>(val));
    }

    let slice = &mut data[Self::START_IDX..=Self::END_IDX];
    let old_val = Self::READ(slice.try_into().unwrap());
    let new_val = old_val & (Self::MASK_LHS ^ Self::MASK_RHS as u16) | val << (u16::BITS as usize - BITS - Self::OFFSET);
    slice.copy_from_slice(&Self::WRITE(new_val));
    Ok(())
  }

  fn get_bits(data: &[u8]) -> u16 {
    let _ = Self::CHECK_;

    let slice = &data[Self::START_IDX..=Self::END_IDX];
    let val = Self::READ(slice.try_into().unwrap());
    // println!("lhs = {:016b}, rhs = {:016b}, lhs ^ rhs = {:016b}", Self::MASK_LHS, Self::MASK_RHS, Self::MASK_LHS ^ Self::MASK_RHS as u16);
    // println!("{:016b}", !(Self::MASK_LHS ^ Self::MASK_RHS as u16));
    // println!("val = {:016b}, res={:016b}", val, (val & !(Self::MASK_LHS ^ Self::MASK_RHS as u16)));
    // println!("shift = {}", (u16::BITS as usize - BITS - Self::OFFSET));
    // println!("res={:016b}", (val & !(Self::MASK_LHS ^ Self::MASK_RHS as u16)) >> (u16::BITS as usize - BITS - Self::OFFSET));
    (val & !(Self::MASK_LHS ^ Self::MASK_RHS as u16)) >> (u16::BITS as usize - BITS - Self::OFFSET)
  }

  fn set_bits_accross(
      data: &mut [u8], val: u16) -> anyhow::Result<()> {
    let _ = Self::CHECK_;

    if !Self::does_fit(val) {
      bail!(OverflowError::<BITS, u16>(val));
    }

    let lhs_slice = &mut data[Self::START_IDX..Self::END_IDX];
    let lhs = Self::READ(lhs_slice.try_into().unwrap());
    let new_lhs = lhs & Self::MASK_LHS | val >> (BITS + Self::OFFSET - u16::BITS as usize);
    lhs_slice.copy_from_slice(&Self::WRITE(new_lhs));

    let rhs_slice = &mut data[Self::END_IDX..=Self::END_IDX];
    let rhs = u8::from_ne_bytes(rhs_slice.try_into().unwrap());
    let new_rhs = rhs & Self::MASK_RHS | ((val & u8::MAX as u16) as u8) << (u8::BITS as usize - ((BITS + Self::OFFSET) % 8));
    rhs_slice.copy_from_slice(&new_rhs.to_ne_bytes());

    Ok(())
  }

  fn get_bits_accross(data: &[u8]) -> u16 {
    let _ = Self::CHECK_;

    let lhs_slice = &data[Self::START_IDX..Self::END_IDX];
    let lhs = Self::READ(lhs_slice.try_into().unwrap());
    let lhs = (lhs & !Self::MASK_LHS) << (BITS + Self::OFFSET - u16::BITS as usize);

    let rhs_slice = &data[Self::END_IDX..=Self::END_IDX];
    let rhs = u8::from_ne_bytes(rhs_slice.try_into().unwrap());
    let rhs = (rhs & !Self::MASK_RHS) >> (u8::BITS as usize - ((BITS + Self::OFFSET) % 8));

    lhs | (rhs as u16)
  }
}

struct BitsU32<const BITS: usize, const START: usize, const SIZE: usize>;

impl<const BITS: usize, const START: usize, const SIZE: usize> SetGet<BITS, START, SIZE> for BitsU32<BITS, START, SIZE> {
  type Target = u32;

  const MASK_LHS: Self::Target = if Self::OFFSET == 0 { 0 }
    else { u32::MAX << (u32::BITS as usize - Self::OFFSET) };

  const IS_ACROSS: bool = Self::OFFSET + BITS > u32::BITS as usize;
  const GET: fn(&[u8]) -> Self::Target = if Self::IS_ACROSS {
    Self::get_bits_accross
  } else {
    Self::get_bits
  };
  const SET: fn(&mut [u8], Self::Target) -> anyhow::Result<()> = if Self::IS_ACROSS {
    Self::set_bits_accross
  } else {
    Self::set_bits
  };
}

impl<const BITS: usize, const START: usize, const SIZE: usize> BitsU32<BITS, START, SIZE> {
  const CHECK_: () = [()][(BITS > u32::BITS as usize ||
                           BITS <= u16::BITS as usize) as usize];
  const READ: fn([u8; 4]) -> u32 = if cfg!(target_endian = "big") {
    u32::from_le_bytes
  } else {
    u32::from_be_bytes
  };
  const WRITE: fn(u32) -> [u8; 4] = if cfg!(target_endian = "big") {
    u32::to_le_bytes
  } else {
    u32::to_be_bytes
  };

  const fn does_fit(val: u32) -> bool {
    let val_num_bits = (u32::BITS - val.leading_zeros()) as usize;
    val_num_bits <= BITS
  }

  fn set_bits(data: &mut [u8], val: u32) -> anyhow::Result<()> {
    let _ = Self::CHECK_;
    if !Self::does_fit(val) {
      bail!(OverflowError::<BITS, u32>(val));
    }

    let slice = &mut data[Self::START_IDX..=Self::END_IDX];
    let old_val = Self::READ(slice.try_into().unwrap());
    let new_val = old_val & (Self::MASK_LHS ^ Self::MASK_RHS as u32) | val << (u32::BITS as usize - BITS - Self::OFFSET);
    slice.copy_from_slice(&Self::WRITE(new_val));
    Ok(())
  }

  fn get_bits(data: &[u8]) -> u32 {
    let _ = Self::CHECK_;

    let slice = &data[Self::START_IDX..=Self::END_IDX];
    let val = Self::READ(slice.try_into().unwrap());
    // println!("lhs = {:016b}, rhs = {:016b}, lhs ^ rhs = {:016b}", Self::MASK_LHS, Self::MASK_RHS, Self::MASK_LHS ^ Self::MASK_RHS as u16);
    // println!("{:016b}", !(Self::MASK_LHS ^ Self::MASK_RHS as u16));
    // println!("val = {:016b}, res={:016b}", val, (val & !(Self::MASK_LHS ^ Self::MASK_RHS as u16)));
    // println!("shift = {}", (u16::BITS as usize - BITS - Self::OFFSET));
    // println!("res={:016b}", (val & !(Self::MASK_LHS ^ Self::MASK_RHS as u16)) >> (u16::BITS as usize - BITS - Self::OFFSET));
    (val & !(Self::MASK_LHS ^ Self::MASK_RHS as u32)) >> (u32::BITS as usize - BITS - Self::OFFSET)
  }

  fn set_bits_accross(
      data: &mut [u8], val: u32) -> anyhow::Result<()> {
    let _ = Self::CHECK_;

    if !Self::does_fit(val) {
      bail!(OverflowError::<BITS, u32>(val));
    }

    let lhs_slice = &mut data[Self::START_IDX..Self::END_IDX];
    let lhs = Self::READ(lhs_slice.try_into().unwrap());
    let new_lhs = lhs & Self::MASK_LHS | val >> (BITS + Self::OFFSET - u32::BITS as usize);
    lhs_slice.copy_from_slice(&Self::WRITE(new_lhs));

    let rhs_slice = &mut data[Self::END_IDX..=Self::END_IDX];
    let rhs = u8::from_ne_bytes(rhs_slice.try_into().unwrap());
    let new_rhs = rhs & Self::MASK_RHS | ((val & u8::MAX as u32) as u8) << (u8::BITS as usize - ((BITS + Self::OFFSET) % 8));
    rhs_slice.copy_from_slice(&new_rhs.to_ne_bytes());

    Ok(())
  }

  fn get_bits_accross(data: &[u8]) -> u32 {
    let _ = Self::CHECK_;

    let lhs_slice = &data[Self::START_IDX..Self::END_IDX];
    println!("lhs_slice size: {}", lhs_slice.len());
    let lhs = Self::READ(lhs_slice.try_into().unwrap());
    let lhs = (lhs & !Self::MASK_LHS) << (BITS + Self::OFFSET - u32::BITS as usize);

    let rhs_slice = &data[Self::END_IDX..=Self::END_IDX];
    let rhs = u8::from_ne_bytes(rhs_slice.try_into().unwrap());
    let rhs = (rhs & !Self::MASK_RHS) >> (u8::BITS as usize - ((BITS + Self::OFFSET) % 8));

    lhs | (rhs as u32)
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_get_bits_u8() {
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
    assert_eq!(BitsU8::<3, 20, 5>::GET(&data), 0b00000001);

    assert_eq!(BitsU8::<8, 32, 5>::END_IDX, 4);
    assert_eq!(BitsU8::<8, 32, 5>::START_IDX, 4);
    assert_eq!(BitsU8::<8, 32, 5>::OFFSET, 0);
    assert_eq!(BitsU8::<8, 32, 5>::MASK_LHS, 0b0000000);
    assert_eq!(BitsU8::<8, 32, 5>::MASK_RHS, 0b0000000);
    assert_eq!(BitsU8::<8, 32, 5>::GET(&data), 0b00111100);

    // Accross u8
    assert_eq!(BitsU8::<8, 20, 5>::END_IDX, 3);
    assert_eq!(BitsU8::<8, 20, 5>::START_IDX, 2);
    assert_eq!(BitsU8::<8, 20, 5>::OFFSET, 4);
    assert_eq!(BitsU8::<8, 20, 5>::MASK_LHS, 0b11110000);
    assert_eq!(BitsU8::<8, 20, 5>::MASK_RHS, 0b00001111);
    assert_eq!(BitsU8::<8, 20, 5>::GET(&data), 0b00111100);

    assert_eq!(BitsU8::<2, 7, 5>::END_IDX, 1);
    assert_eq!(BitsU8::<2, 7, 5>::START_IDX, 0);
    assert_eq!(BitsU8::<2, 7, 5>::OFFSET, 7);
    assert_eq!(BitsU8::<2, 7, 5>::MASK_LHS, 0b11111110);
    assert_eq!(BitsU8::<2, 7, 5>::MASK_RHS, 0b01111111);
    assert_eq!(BitsU8::<2, 7, 5>::GET(&data), 0b00000001);
  }

  #[test]
  fn test_get_bits_u16() {
    let data = [0xAA, 0xF0, 0x33, 0xCC, 0x3C];

    // Inside 2 u8s
    assert_eq!(BitsU16::<10, 20, 5>::END_IDX, 3);
    assert_eq!(BitsU16::<10, 20, 5>::START_IDX, 2);
    assert_eq!(BitsU16::<10, 20, 5>::OFFSET, 4);
    assert_eq!(BitsU16::<10, 20, 5>::MASK_LHS, 0xF000);
    assert_eq!(BitsU16::<10, 20, 5>::MASK_RHS, 0b00000011);
    assert_eq!(BitsU16::<10, 20, 5>::GET(&data), 0x00F3);

    assert_eq!(BitsU16::<16, 24, 5>::END_IDX, 4);
    assert_eq!(BitsU16::<16, 24, 5>::START_IDX, 3);
    assert_eq!(BitsU16::<16, 24, 5>::OFFSET, 0);
    assert_eq!(BitsU16::<16, 24, 5>::MASK_LHS, 0x0000);
    assert_eq!(BitsU16::<16, 24, 5>::MASK_RHS, 0b0000000);
    assert_eq!(BitsU16::<16, 24, 5>::GET(&data), 0xCC3C);

    // Accross 3 u8s
    assert_eq!(BitsU16::<16, 20, 5>::END_IDX, 4);
    assert_eq!(BitsU16::<16, 20, 5>::START_IDX, 2);
    assert_eq!(BitsU16::<16, 20, 5>::OFFSET, 4);
    assert_eq!(BitsU16::<16, 20, 5>::MASK_LHS, 0xF000);
    assert_eq!(BitsU16::<16, 20, 5>::MASK_RHS, 0b00001111);
    assert_eq!(BitsU16::<16, 20, 5>::GET(&data), 0x3CC3);

    assert_eq!(BitsU16::<10, 7, 5>::END_IDX, 2);
    assert_eq!(BitsU16::<10, 7, 5>::START_IDX, 0);
    assert_eq!(BitsU16::<10, 7, 5>::OFFSET, 7);
    assert_eq!(BitsU16::<10, 7, 5>::MASK_LHS, 0xFE00);
    assert_eq!(BitsU16::<10, 7, 5>::MASK_RHS, 0b01111111);
    assert_eq!(BitsU16::<10, 7, 5>::GET(&data), 0x01E0);
  }

  #[test]
  fn test_set_bits_u16() {
    let mut data = [0xAA, 0xF0, 0x33, 0xCC, 0x3C];

    // Inside 2 u8s
    assert!(BitsU16::<10, 16, 5>::SET(&mut data, 0x0156).is_ok());
    assert_eq!(BitsU16::<10, 16, 5>::GET(&data), 0x0156);

    assert!(BitsU16::<9, 3, 5>::SET(&mut data, 0x015D).is_ok());
    assert_eq!(BitsU16::<9, 3, 5>::GET(&data), 0x015D);

    // Accross 3 u8s
    assert!(BitsU16::<16, 20, 5>::SET(&mut data, 0xD528).is_ok());
    assert_eq!(BitsU16::<16, 20, 5>::GET(&data), 0xD528);

    assert!(BitsU16::<10, 7, 5>::SET(&mut data, 0x02B7).is_ok());
    assert_eq!(BitsU16::<10, 7, 5>::GET(&data), 0x02B7);

    assert_eq!(data, [0xB5, 0x5B, 0xDD, 0x52, 0x8C]);
  }

  #[test]
  fn test_get_bits_u32() {
    let data = [0xAA, 0xF0, 0x33, 0xCC, 0x3C];

    // Inside 4 u8s
    assert_eq!(BitsU32::<18, 7, 5>::END_IDX, 3);
    assert_eq!(BitsU32::<18, 7, 5>::START_IDX, 0);
    assert_eq!(BitsU32::<18, 7, 5>::OFFSET, 7);
    assert_eq!(BitsU32::<18, 7, 5>::MASK_LHS, 0xFE000000);
    assert_eq!(BitsU32::<18, 7, 5>::MASK_RHS, 0b01111111);
    assert_eq!(BitsU32::<18, 7, 5>::GET(&data), 0x0001E067);

    // Accross 5 u8s
    assert_eq!(BitsU32::<29, 6, 5>::END_IDX, 4);
    assert_eq!(BitsU32::<29, 6, 5>::START_IDX, 0);
    assert_eq!(BitsU32::<29, 6, 5>::OFFSET, 6);
    assert_eq!(BitsU32::<29, 6, 5>::MASK_LHS, 0xFC000000);
    assert_eq!(BitsU32::<29, 6, 5>::MASK_RHS, 0b00011111);
    assert_eq!(BitsU32::<29, 6, 5>::GET(&data), 0x17819E61);
  }

  #[test]
  fn test_set_bits_u32() {
    let mut data = [0xAA, 0xF0, 0x33, 0xCC, 0x3C];

    // Inside 4 u8s
    assert!(BitsU32::<32, 0, 5>::SET(&mut data, 0xC75B0814).is_ok());
    assert_eq!(BitsU32::<32, 0, 5>::GET(&data), 0xC75B0814);

    assert_eq!(data, [0xC7, 0x5B, 0x08, 0x14, 0x3C]);

    // Accross 5 u8s
    assert!(BitsU32::<32, 7, 5>::SET(&mut data, 0x2036D751).is_ok());
    assert_eq!(BitsU32::<32, 7, 5>::GET(&data), 0x2036D751);

    assert_eq!(data, [0xC6, 0x40, 0x6D, 0xAE, 0xA2]);
  }

  #[test]
  fn test_set_bits_u8() {
    let mut data = [
      0b10101010,
      0b11110000,
      0b00110011,
      0b11001100,
      0b00111100];

    // Inside u8
    assert_eq!(BitsU8::<4, 16, 5>::GET(&data), 0b00000011);
    assert!(BitsU8::<4, 16, 5>::SET(&mut data, 0b00001010).is_ok());
    assert_eq!(BitsU8::<4, 16, 5>::GET(&data), 0b00001010);

    assert_eq!(BitsU8::<8, 0, 5>::GET(&data), 0b10101010);
    assert!(BitsU8::<8, 0, 5>::SET(&mut data, 0b11100011).is_ok());
    assert_eq!(BitsU8::<8, 0, 5>::GET(&data), 0b11100011);

    // Accross u8
    assert_eq!(BitsU8::<8, 20, 5>::GET(&data), 0b00111100);
    assert!(BitsU8::<8, 20, 5>::SET(&mut data, 0b01001001).is_ok());
    assert_eq!(BitsU8::<8, 20, 5>::GET(&data), 0b01001001);

    assert_eq!(BitsU8::<2, 7, 5>::GET(&data), 0b00000011);
    assert!(BitsU8::<2, 7, 5>::SET(&mut data, 0b00000001).is_ok());
    assert_eq!(BitsU8::<2, 7, 5>::GET(&data), 0b00000001);

    assert_eq!(data, [
      0b11100010,
      0b11110000,
      0b10100100,
      0b10011100,
      0b00111100
    ]);
  }
}
