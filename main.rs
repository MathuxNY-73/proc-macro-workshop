// Write code here.
//
// To see what the code looks like after macro expansion:
//     $ cargo expand
//
// To run the code:
//     $ cargo run

use bitfield::*;

#[bitfield]
pub struct RedirectionTableEntry {
    delivery_mode: DeliveryMode,
    reserved: B5,
}

const F: isize = 3;
const G: isize = 8;

#[derive(BitfieldSpecifier, Debug, PartialEq)]
pub enum DeliveryMode {
    Fixed = F,
    Lowest,
    SMI,
    RemoteRead,
    NMI,
    Init = G,
    Startup,
    External,
}

fn main() {
    assert_eq!(std::mem::size_of::<RedirectionTableEntry>(), 1);

    // Initialized to all 0 bits.
    let mut entry = RedirectionTableEntry::new();
    assert_eq!(entry.get_delivery_mode(), DeliveryMode::Init);

    entry.set_delivery_mode(DeliveryMode::Lowest);
    assert_eq!(entry.get_delivery_mode(), DeliveryMode::Lowest);
}
