use bitbybit::bitfield;
use zerocopy::FromBytes;
use zerocopy_derive::{FromBytes, Immutable, KnownLayout};

#[derive(FromBytes, Immutable, KnownLayout)]
#[repr(C)]
pub struct GbaCartridgeHeader {
    pub entry_point: u32,
    // Compressed bitmap, required
    // TODO: should crash if doesn't match bios
    pub logo: [u8; 156],
    // ASCII
    pub title: [u8; 12],
    // U (unique code), TT (short title), D (destination/language)
    pub game_code: [u8; 4],
    // Identifiers who made the game
    pub maker_code: [u8; 2],
    // Must be 0x96
    pub fixed_value: u8,
    // 00h for current models
    pub main_unit_code: u8,
    // usually 00h
    pub device_type: u8,
    // Should be 0 filled
    pub reserved: [u8; 7],
    // Software version, usually 0x00
    pub software_version: u8,
    // Complement check, header checksum, required!
    pub complement_check: u8,
    // Should be 0 filled
    pub reserved2: [u8; 2],
}

// Sits at 8000000h in ROM
#[derive(FromBytes, Immutable, KnownLayout)]
#[repr(C)]
pub struct GbaCartridge {
    pub header: GbaCartridgeHeader,
    pub body: [u8],
}

impl GbaCartridge {
    fn test() {
        let mut bytes = [0u8; 192];
        let cartridge = GbaCartridge::ref_from_bytes(&bytes);
    }
}
