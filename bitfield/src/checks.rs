pub trait TotalSizeIsMultipleOfEightBits {
  type Check;
}

pub type MultipleOfEight<T> = <<T as ByteArray>::Marker as TotalSizeIsMultipleOfEightBits>::Check;

pub trait ByteArray {
  type Marker;
}

pub struct SevenMod8;
pub struct SixMod8;
pub struct FiveMod8;
pub struct FourMod8;
pub struct ThreeMod8;
pub struct TwoMod8;
pub struct OneMod8;
pub struct ZeroMod8;

impl ByteArray for [(); 0] {
  type Marker = ZeroMod8;
}
impl ByteArray for [(); 1] {
  type Marker = OneMod8;
}
impl ByteArray for [(); 2] {
  type Marker = TwoMod8;
}
impl ByteArray for [(); 3] {
  type Marker = ThreeMod8;
}
impl ByteArray for [(); 4] {
  type Marker = FourMod8;
}
impl ByteArray for [(); 5] {
  type Marker = FiveMod8;
}
impl ByteArray for [(); 6] {
  type Marker = SixMod8;
}
impl ByteArray for [(); 7] {
  type Marker = SevenMod8;
}

impl TotalSizeIsMultipleOfEightBits for ZeroMod8 {
  type Check = ();
}

// pub type IsDiscriminantInRange<T> = <<T as BoolArray>::Marker as DiscriminantInRange>;

pub trait BoolArray {
  type Marker;
}

pub trait DiscriminantInRange {
  const CHECK: () = ();
}

pub struct False;
pub struct True;

impl BoolArray for [(); 0] {
  type Marker = True;
}

impl BoolArray for [(); 1] {
  type Marker = False;
}

impl DiscriminantInRange for True {}
