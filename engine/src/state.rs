use arbitrary_int::u19;
use bitbybit::{bitenum, bitfield};
use zerocopy_derive::IntoBytes;

#[bitenum(u3, exhaustive = true)]
#[derive(IntoBytes)]
#[repr(u8)]
pub enum Bank {
    None = 0,
    Fiq = 1,
    Svc = 2,
    Abt = 3,
    Irq = 4,
    Und = 5,
    Invalid = 6,
    Count = 7,
}

#[bitenum(u5, exhaustive = false)]
#[derive(IntoBytes)]
#[repr(u8)]
pub enum Mode {
    Unknown = 0x00,
    User = 0x10,
    Fiq = 0x11,
    Irq = 0x12,
    Supervisor = 0x13,
    Abort = 0x17,
    Undefined = 0x1B,
    System = 0x1F,
}

#[bitenum(u1, exhaustive = true)]
#[derive(IntoBytes)]
#[repr(u8)]
pub enum ThumbState {
    ARM = 0x00,
    Thumb = 0x01,
}

#[bitfield(u32)]
#[derive(IntoBytes)]
pub struct StatusRegister {
    #[bits(0..=4, rw)]
    pub mode: Option<Mode>,
    #[bit(5, rw)]
    pub thumb_state: ThumbState,
    #[bit(6, rw)]
    pub mask_fiq: bool,
    #[bit(7, rw)]
    pub mask_irq: bool,
    #[bits(8..=26, rw)]
    pub reserved: u19,
    #[bit(27, rw)]
    pub sticky_overflow: bool, // Q flag
    #[bit(28, rw)]
    pub overflow: bool, // V flag
    #[bit(29, rw)]
    pub carry: bool, // C flag
    #[bit(30, rw)]
    pub zero: bool, // Z flag
    #[bit(31, rw)]
    pub negative: bool, // N flag
}

pub struct RegisterFile {
    reg: [u32; 16],
    // Banked registers for FIQ mode
    // i.e. R8-R12
    fiq_bank: [u32; 5],
    // All registers R13 & R14 are banked
    r13_bank: [u32; Bank::Count as usize],
    r14_bank: [u32; Bank::Count as usize],

    cpsr: StatusRegister,
    spsr: [StatusRegister; Bank::Count as usize],
}

impl RegisterFile {
    pub fn new() -> Self {
        // Initial cpsr state.
        let cpsr = StatusRegister::builder()
            .with_mode(Mode::System)
            .with_thumb_state(ThumbState::ARM)
            .with_mask_fiq(true)
            .with_mask_irq(true)
            .with_reserved(u19::new(0))
            .with_sticky_overflow(false)
            .with_overflow(false)
            .with_carry(false)
            .with_zero(false)
            .with_negative(false)
            .build();

        RegisterFile {
            reg: [0; 16],
            fiq_bank: [0; 5],
            r13_bank: [0; Bank::Count as usize],
            r14_bank: [0; Bank::Count as usize],
            cpsr,
            spsr: [StatusRegister::ZERO; Bank::Count as usize],
        }
    }

    pub fn reset(&mut self) {
        self.reg = [0; 16];
        self.fiq_bank = [0; 5];
        self.r13_bank = [0; Bank::Count as usize];
        self.r14_bank = [0; Bank::Count as usize];
        // TODO: Is this right??
        self.cpsr = StatusRegister::ZERO;
        self.spsr = [StatusRegister::ZERO; Bank::Count as usize];
    }
}
