use anyhow::Result;

pub enum GamePakMemory {
    Rom { data: Vec<u8> },   // Read-only memory, typically the game code
    Flash { data: Vec<u8> }, // Writable memory, used for saving game state
}

// This is the memory mapping for our GBA.
// We store it using just vectors of u8, because modern systems can easily handle
//
pub struct Memory {
    pub bios: [u8; 16 * 1024],               // 16kB BIOS ROM
    pub external_work_ram: [u8; 256 * 1024], // 256kB
    pub internal_work_ram: [u8; 32 * 1024],  // 32kB
    // Technically this is only 1023 bytes but this is fine!
    pub io_registers: [u8; 1024],            // ~1kB
    pub palette_ram: [u8; 1024],             // 1kB
    pub vram: [u8; 96 * 1024],               // 96kB
    pub oam: [u8; 1024],                     // 1kB
    pub game_pak_memory: [GamePakMemory; 3], // 3 wait states, (max 32MB each)
    pub game_pak_sram: Vec<u8>,              // Max 64kbs SRAM
}

impl Memory {
    pub fn new(game_pak_memory: [GamePakMemory; 3], game_pak_sram_size: usize) -> Result<Self> {
        if game_pak_sram_size > 64 * 1024 {
            return Err(anyhow::anyhow!(
                "GamePak SRAM size cannot exceed 64kB, got {}",
                game_pak_sram_size
            ));
        }
        for game_pak_memory in game_pak_memory.iter() {
            match game_pak_memory {
                GamePakMemory::Rom { data } | GamePakMemory::Flash { data } => {
                    if data.len() > 32 * 1024 * 1024 {
                        return Err(anyhow::anyhow!(
                            "GamePak ROM/Flash size cannot exceed 32MB, got {}",
                            data.len()
                        ));
                    }
                }
            }
        }

        Ok(Memory {
            bios: [0; 16 * 1024],
            external_work_ram: [0; 256 * 1024],
            internal_work_ram: [0; 32 * 1024],
            io_registers: [0; 1024],
            palette_ram: [0; 1024],
            vram: [0; 96 * 1024],
            oam: [0; 1024],
            game_pak_memory: game_pak_memory,
            game_pak_sram: vec![0; game_pak_sram_size],
        })
    }

    pub fn get_address_space(&self, address: u32) -> (&[u8], Region) {
        match address {
            0x00000000..=0x00003FFF => (&self.bios, Region::Bios),
            0x02000000..=0x0203FFFF => (&self.external_work_ram, Region::ExternalWorkRam),
            0x03000000..=0x03007FFF => (&self.internal_work_ram, Region::InternalWorkRam),
            0x04000000..=0x040003FF => (&self.io_registers, Region::IORegisters),
            0x05000000..=0x050003FF => (&self.palette_ram, Region::PaletteRam),
            0x06000000..=0x06017FFF => (&self.vram, Region::Vram),
            0x07000000..=0x070003FF => (&self.oam, Region::Oam),
            0x08000000..=0x0DFFFFFF => {
                let index = ((address - 0x08000000) / (32 * 1024)) as u8;
                match &self.game_pak_memory[index as usize] {
                    GamePakMemory::Rom { data } => (data, Region::GamePakRom { wait_state: index }),
                    GamePakMemory::Flash { data } => {
                        (data, Region::GamePakFlash { wait_state: index })
                    }
                }
            }
            0x0E000000..=0x0E00FFFF => (&self.game_pak_sram, Region::GamePakSRAM),
            _ => (&[], Region::Unknown),
        }
    }

    pub fn read_byte(&self, address: u32) -> u8 {
        // Placeholder for actual memory read logic
        0
    }

    pub fn write_byte(&mut self, address: u32, value: u8) {
        // Placeholder for actual memory write logic
    }

    pub fn read_word(&self, address: u32) -> u32 {
        // Placeholder for actual memory read logic
        0
    }

    pub fn write_word(&mut self, address: u32, value: u32) {
        // Placeholder for actual memory write logic
    }
}

// Timing data probably can't be generally set, because there are lots of cases
// such as accessing display memory adding cycles or waitstate settings changing this.
// It's probably easier to just hook in sleeps manually when we do lookups on this memory.
pub enum Region {
    // BIOS Rom, 16kbs              (0x00000000 - 0x00003FFF)
    Bios,
    // On-board Work RAM, 256kbs    (0x02000000 - 0x0203FFFF)
    ExternalWorkRam,
    // On-chip Work RAM, 32kbs      (0x03000000 - 0x03007FFF)
    InternalWorkRam,
    // IO Registers, ~1kb           (0x04000000 - 0x040003FF)
    IORegisters,
    // Palette RAM, 1kb             (0x05000000 - 0x050003FF)
    PaletteRam,
    // VRAM, 96kb                   (0x06000000 - 0x06017FFF)
    Vram,
    // OAM, 1kb                     (0x07000000 - 0x070003FF)
    Oam,
    // GamePak ROM (max 32 MBs)     (0x08000000 - 0x0DFFFFFF)
    // Wait State will effect timings
    GamePakRom { wait_state: u8 },
    // Is writable!
    GamePakFlash { wait_state: u8 },
    // GamePak SRAM, 64kbs          (0x0E000000 - 0x0E00FFFF)
    GamePakSRAM,
    // Invalid address, or a not used memory space.
    Unknown,
}

bitflags::bitflags! {
    pub struct BusWidth: u8 {
        const BIT_8 = 0b00000001;
        const BIT_16 = 0b00000010;
        const BIT_32 = 0b00000100;
    }
}

impl Region {
    pub fn is_readable(&self) -> bool {
        match self {
            Region::Unknown => false,
            _ => true,
        }
    }

    pub fn is_writable(&self) -> bool {
        // The only 2 non writable regions are the BIOS and GamePak ROM (and obviously unknown addresses).
        match self {
            Region::Unknown => false,
            Region::Bios | Region::GamePakRom { .. } => false,
            _ => true,
        }
    }

    pub fn bus_widths_write(&self) -> BusWidth {
        match self {
            Region::Bios | Region::GamePakRom { .. } => BusWidth::empty(),
            Region::ExternalWorkRam | Region::InternalWorkRam | Region::IORegisters => {
                BusWidth::BIT_8 | BusWidth::BIT_16 | BusWidth::BIT_32
            }
            Region::PaletteRam | Region::Vram | Region::Oam | Self::GamePakFlash { .. } => {
                BusWidth::BIT_16 | BusWidth::BIT_32
            }
            Region::GamePakSRAM => BusWidth::BIT_8,
            Region::Unknown => BusWidth::empty(),
        }
    }

    pub fn bus_widths_read(&self) -> BusWidth {
        match self {
            Region::Bios
            | Region::GamePakRom { .. }
            | Region::ExternalWorkRam
            | Region::InternalWorkRam
            | Region::IORegisters
            | Region::PaletteRam
            | Region::Vram
            | Region::Oam
            | Self::GamePakFlash { .. } => BusWidth::BIT_8 | BusWidth::BIT_16 | BusWidth::BIT_32,
            Region::GamePakSRAM => BusWidth::BIT_8,
            Region::Unknown => BusWidth::empty(),
        }
    }
}
