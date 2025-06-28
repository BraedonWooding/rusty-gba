use arbitrary_int::prelude::*;
use bitbybit::{bitenum, bitfield};
use bitpatterns::{BitPattern, bitpattern};
use zerocopy_derive::IntoBytes;

#[bitenum(u4, exhaustive = true)]
#[derive(IntoBytes)]
#[repr(u8)]
pub enum Register {
    R0 = 0,
    R1 = 1,
    R2 = 2,
    R3 = 3,
    R4 = 4,
    R5 = 5,
    R6 = 6,
    R7 = 7,
    R8 = 8,
    R9 = 9,
    R10 = 10,
    R11 = 11,
    R12 = 12,
    SP = 13, // Stack Pointer
    LR = 14, // Link Register
    PC = 15, // Program Counter
}

impl Register {
    pub fn to_string(&self) -> &'static str {
        match self {
            Register::R0 => "R0",
            Register::R1 => "R1",
            Register::R2 => "R2",
            Register::R3 => "R3",
            Register::R4 => "R4",
            Register::R5 => "R5",
            Register::R6 => "R6",
            Register::R7 => "R7",
            Register::R8 => "R8",
            Register::R9 => "R9",
            Register::R10 => "R10",
            Register::R11 => "R11",
            Register::R12 => "R12",
            // Guessing I can use SP/LR/PC here
            Register::SP => "SP",
            Register::LR => "LR",
            Register::PC => "PC",
        }
    }
}

#[derive(Debug, IntoBytes)]
#[bitenum(u4, exhaustive = true)]
#[repr(u8)]
pub enum Condition {
    Equal = 0,                  // EQ (Z = 1, same)
    NotEqual = 1,               // NE (Z = 0, not same)
    UnsignedGreaterOrEqual = 2, // CS/HS (CarrySet = 1)
    UnsignedLess = 3,           // CC/LO (CarrySet = 0)
    SignedNegative = 4,         // MI (N = 1, negative)
    SignedPositiveOrZero = 5,   // PL (N = 0, positive or zero)
    SignedOverflow = 6,         // VS (V = 1, overflow)
    SignedNoOverflow = 7,       // VC (V = 0, no overflow)
    UnsignedHigher = 8,         // HI (C = 1 and Z = 0)
    UnsignedLessOrEqual = 9,    // LS (C = 0 or Z = 1)
    SignedGreaterOrEqual = 10,  // GE (N = V)
    SignedLess = 11,            // LT (N != V)
    SignedGreater = 12,         // GT (Z = 0 and N = V)
    SignedLessOrEqual = 13,     // LE (Z = 1 or N != V)
    Always = 14,                // AL (always execute)
    Never = 15,                 // NV (never execute)
}

impl Condition {
    pub fn suffix(&self) -> &'static str {
        match self {
            Condition::Equal => "eq",
            Condition::NotEqual => "ne",
            Condition::UnsignedGreaterOrEqual => "cs",
            Condition::UnsignedLess => "cc",
            Condition::SignedNegative => "mi",
            Condition::SignedPositiveOrZero => "pl",
            Condition::SignedOverflow => "vs",
            Condition::SignedNoOverflow => "vc",
            Condition::UnsignedHigher => "hi",
            Condition::UnsignedLessOrEqual => "ls",
            Condition::SignedGreaterOrEqual => "ge",
            Condition::SignedLess => "lt",
            Condition::SignedGreater => "gt",
            Condition::SignedLessOrEqual => "le",
            Condition::Always => "al",
            Condition::Never => "nv",
        }
    }
}

const DATA_PROCESSING_IMMEDIATE_BIT_PATTERN: BitPattern<u32> =
    bitpattern!("0b...._001._...._...._...._...._...._....");
const DATA_PROCESSING_REGISTER_SHIFTED_BY_IMMEDIATE_BIT_PATTERN: BitPattern<u32> =
    bitpattern!("0b...._000._...._...._...._...._...._0...");
const DATA_PROCESSING_REGISTER_SHIFTED_BY_REGISTER_BIT_PATTERN: BitPattern<u32> =
    bitpattern!("0b...._000._...._...._...._...._...._1...");
const STATUS_TRANSFER_TO_REGISTER_BIT_PATTERN: BitPattern<u32> =
    bitpattern!("0b...._0001_0.00_1111_...._0000_0000_0000");
const STATUS_TRANSFER_FROM_REGISTER_BIT_PATTERN: BitPattern<u32> =
    bitpattern!("0b...._0001_0.10_...._1111_0000_0000_....");
const STATUS_TRANSFER_FROM_IMMEDIATE_BIT_PATTERN: BitPattern<u32> =
    bitpattern!("0b...._0011_0.10_...._1111_...._...._....");
const MULTIPLY_BIT_PATTERN: BitPattern<u32> =
    bitpattern!("0b...._0000_00.._...._...._...._1001_....");
const MULTIPLY_LONG_BIT_PATTERN: BitPattern<u32> =
    bitpattern!("0b...._0000_1..._...._...._...._1001_....");
const SINGLE_DATA_SWAP_BIT_PATTERN: BitPattern<u32> =
    bitpattern!("0b...._0001_0.00_...._...._0000_1001_....");
const BRANCH_AND_EXCHANGE_BIT_PATTERN: BitPattern<u32> =
    bitpattern!("0b...._0001_0010_1111_1111_1111_0001_....");
const HALFWORD_DATA_TRANSFER_REGISTER_LOAD_BIT_PATTERN: BitPattern<u32> =
    bitpattern!("0b...._000._.0.1_...._...._0000_1..1_....");
const HALFWORD_DATA_TRANSFER_IMMEDIATE_LOAD_BIT_PATTERN: BitPattern<u32> =
    bitpattern!("0b...._000._.1.1_...._...._...._1..1_....");
const HALFWORD_DATA_TRANSFER_REGISTER_STORE_BIT_PATTERN: BitPattern<u32> =
    bitpattern!("0b...._000._.0.0_...._...._0000_1..1_....");
const HALFWORD_DATA_TRANSFER_IMMEDIATE_STORE_BIT_PATTERN: BitPattern<u32> =
    bitpattern!("0b...._000._.1.0_...._...._...._1..1_....");
const UNDEFINED_BIT_PATTERN: BitPattern<u32> =
    bitpattern!("0b...._011._...._...._...._...._...1_....");
// Note: above will collide with the single data transfer bit pattern
// so the ordering of the patterns is important.
const SINGLE_DATA_TRANSFER_IMMEDIATE_BIT_PATTERN: BitPattern<u32> =
    bitpattern!("0b...._010._...._...._...._...._...._....");
const SINGLE_DATA_TRANSFER_SHIFTED_REGISTER_BIT_PATTERN: BitPattern<u32> =
    bitpattern!("0b...._011._...._...._...._...._...._....");
const BLOCK_DATA_TRANSFER_BIT_PATTERN: BitPattern<u32> =
    bitpattern!("0b...._100._...._...._...._...._...._....");
const BRANCH_BIT_PATTERN: BitPattern<u32> =
    bitpattern!("0b...._101._...._...._...._...._...._....");
const SOFTWARE_INTERRUPT_BIT_PATTERN: BitPattern<u32> =
    bitpattern!("0b...._1111_...._...._...._...._...._....");

// TODO: This default value may be wrong.
#[bitfield(u32)]
#[derive(Debug, IntoBytes)]
pub struct Undefined {
    #[bits(0..=27, rw)]
    pub unused: u28, // All bits are unused in this case
    #[bits(28..=31, rw)]
    pub condition: Condition,
}

// MRS
#[bitfield(u32)]
#[derive(Debug, IntoBytes)]
pub struct StatusTransferToRegister {
    // Must be 0
    #[bits(0..=11, rw)]
    pub unused: u12,
    #[bits(12..=15, rw)]
    pub rd_dest: Register,
    // Must be 0b01111
    #[bits(16..=19, rw)]
    pub unused2: u4,
    // This is MRS, 0
    #[bit(21, rw)]
    pub status_transfer_opcode: StatusTransferOpcode,
    #[bit(22, rw)]
    pub psr: StatusTransferPsrMode,
    // Must be 0b00010
    #[bits(23..=27, rw)]
    pub unused3: u5,
    #[bits(28..=31, rw)]
    pub condition: Condition,
}

// MSR
#[bitfield(u32)]
#[derive(Debug, IntoBytes)]
pub struct StatusTransferFromRegister {
    #[bits(0..=3, rw)]
    pub rm_source: Register,
    // Must be 0b111100000000
    #[bits(4..=15, rw)]
    pub unused: u12,
    #[bits(16..=19, rw)]
    pub fields: StatusTransferField,
    // Must be 0
    #[bit(20, rw)]
    pub unused2: bool,
    // This is MSR 1
    #[bit(21, rw)]
    pub status_transfer_opcode: StatusTransferOpcode,
    #[bit(22, rw)]
    pub psr: StatusTransferPsrMode,
    // Must be 0b10
    #[bits(23..=24, rw)]
    pub unused3: u2,
    // Must be 0
    #[bit(25, rw)]
    pub immediate: bool,
    // Must be 0
    #[bits(26..=27, rw)]
    pub unused4: u2,
    #[bits(28..=31, rw)]
    pub condition: Condition,
}

// MSR
#[bitfield(u32)]
#[derive(Debug, IntoBytes)]
pub struct StatusTransferFromImmediate {
    #[bits(0..=7, rw)]
    pub imm: u8,
    #[bits(8..=11, rw)]
    pub imm_shift: u4,
    // Must be 0b1111
    #[bits(12..=15, rw)]
    pub unused: u4,
    #[bits(16..=19, rw)]
    pub fields: StatusTransferField,
    // Must be 0
    #[bit(20, rw)]
    pub unused2: bool,
    // This is MSR 1
    #[bit(21, rw)]
    pub status_transfer_opcode: StatusTransferOpcode,
    #[bit(22, rw)]
    pub psr: StatusTransferPsrMode,
    // Must be 0b10
    #[bits(23..=24, rw)]
    pub unused3: u2,
    // Must be 1
    #[bit(25, rw)]
    pub immediate: bool,
    // Must be 0
    #[bits(26..=27, rw)]
    pub unused4: u2,
    #[bits(28..=31, rw)]
    pub condition: Condition,
}

bitflags::bitflags! {
    pub struct StatusTransferField : u8 {
        // Confusingly, these have 2 aspects in them, 1) the static unused

        const CONTROL   = 0b0001; // C
        const EXTENSION = 0b0010; // X
        const STATUS    = 0b0100; // S
        const FLAGS     = 0b1000; // F
    }
}

impl ToString for StatusTransferField {
    fn to_string(&self) -> String {
        let mut result = "_".to_string();
        if self.contains(StatusTransferField::CONTROL) {
            result.push('c');
        }
        if self.contains(StatusTransferField::EXTENSION) {
            result.push('x');
        }
        if self.contains(StatusTransferField::STATUS) {
            result.push('s');
        }
        if self.contains(StatusTransferField::FLAGS) {
            result.push('f');
        }
        if result == "_" { String::new() } else { result }
    }
}

impl StatusTransferField {
    pub const fn new_with_raw_value(value: UInt<u8, 4>) -> Self {
        // We only allow the bits that are actually used to be set.
        // The rest will be ignored.
        Self::from_bits_truncate(value.value())
    }

    pub const fn raw_value(&self) -> UInt<u8, 4> {
        u4::new(self.bits())
    }
}

#[bitenum(u1, exhaustive = true)]
#[derive(Debug, IntoBytes, PartialEq, Eq)]
#[repr(u8)]
pub enum StatusTransferOpcode {
    // MRS
    MoveToRegister = 0,
    // MSR
    MoveFromRegister = 1,
}

#[bitenum(u1, exhaustive = true)]
#[derive(Debug, IntoBytes, PartialEq, Eq)]
#[repr(u8)]
pub enum StatusTransferPsrMode {
    // CPSR
    CurrentProgramStatusRegister = 0,
    // SPSR
    SavedProgramStatusRegister = 1,
}

impl StatusTransferPsrMode {
    pub fn suffix(&self) -> &'static str {
        match self {
            StatusTransferPsrMode::CurrentProgramStatusRegister => "cpsr",
            StatusTransferPsrMode::SavedProgramStatusRegister => "spsr",
        }
    }
}

#[bitfield(u32)]
#[derive(Debug, IntoBytes)]
pub struct DataProcessingImmediate {
    #[bits(0..=7, rw)]
    pub operand2: u8,
    #[bits(8..=11, rw)]
    pub operand2_shift: u4,
    #[bits(12..=15, rw)]
    pub rd_dest: Register,
    #[bits(16..=19, rw)]
    pub rn_operand: Register,
    #[bit(20, rw)]
    pub set_condition_codes: bool,
    #[bits(21..=24, rw)]
    pub opcode: DataProcessingOpcode,
    #[bits(25..=27, rw)]
    pub unused: u3,
    #[bits(28..=31, rw)]
    pub condition: Condition,
}

impl DataProcessingImmediate {
    pub fn immediate(&self) -> u32 {
        // We have a 8 bit immediate and a 4 bit shift amount which is a ROR shift (in steps of 2)
        (self.operand2() as u32) << (self.operand2_shift().as_u32() * 2)
    }
}

#[bitfield(u32)]
#[derive(Debug, IntoBytes)]
pub struct DataProcessingRegisterShiftedByImmediate {
    #[bits(0..=3, rw)]
    pub rm_operand2: Register,
    // Always 0 in this case
    #[bit(4, rw)]
    pub shift_by_register: bool,
    #[bits(5..=6, rw)]
    pub shift_type: ShiftType,
    #[bits(7..=11, rw)]
    pub operand2_shift: u5,
    #[bits(12..=15, rw)]
    pub rd_dest: Register,
    #[bits(16..=19, rw)]
    pub rn_operand: Register,
    #[bit(20, rw)]
    pub set_condition_codes: bool,
    #[bits(21..=24, rw)]
    pub opcode: DataProcessingOpcode,
    #[bits(25..=27, rw)]
    pub unused: u3,
    #[bits(28..=31, rw)]
    pub condition: Condition,
}

impl DataProcessingRegisterShiftedByImmediate {
    pub fn get_shift_string(&self) -> String {
        if self.operand2_shift() != u5::new(0) {
            format!(", {}#{}", self.shift_type().suffix(), self.operand2_shift())
        } else {
            match self.shift_type() {
                // TODO: Maybe this needs to be LSL#0?
                ShiftType::Lsl => String::new(),
                ShiftType::Lsr => ", lsr#32".to_string(),
                ShiftType::Asr => ", asr#32".to_string(),
                // Note; this is not ror, it slightly performs differently.
                ShiftType::Ror => ", rrx#32".to_string(),
            }
        }
    }
}

#[bitfield(u32)]
#[derive(Debug, IntoBytes)]
pub struct DataProcessingRegisterShiftedByRegister {
    #[bits(0..=3, rw)]
    pub rm_operand2: Register,
    // Always 1 in this case
    #[bit(4, rw)]
    pub shift_by_register: bool,
    #[bits(5..=6, rw)]
    pub shift_type: ShiftType,
    #[bit(7, rw)]
    pub unused: bool,
    #[bits(8..=11, rw)]
    pub rs_shift: Register,
    #[bits(12..=15, rw)]
    pub rd_dest: Register,
    #[bits(16..=19, rw)]
    pub rn_operand: Register,
    #[bit(20, rw)]
    pub set_condition_codes: bool,
    #[bits(21..=24, rw)]
    pub opcode: DataProcessingOpcode,
    #[bits(25..=27, rw)]
    pub unused_2: u3,
    #[bits(28..=31, rw)]
    pub condition: Condition,
}

#[bitenum(u4, exhaustive = true)]
#[derive(Debug, IntoBytes)]
#[repr(u8)]
pub enum DataProcessingOpcode {
    // AND rd = (Rn AND Op2)
    AndLogical = 0x0,
    // EOR rd = (Rn XOR Op2)
    XorLogical = 0x1,
    // SUB rd = (Rn - Op2)
    Subtract = 0x2,
    // RSB rd = (Op2 - Rn)
    SubtractReversed = 0x3,
    // ADD rd = (Rn + Op2)
    Add = 0x4,
    // ADC rd = (Rn + Op2 + Cy)
    AddWithCarry = 0x5,
    // SBC rd = (Rn - Op2 - Cy - 1)
    SubtractWithCarry = 0x6,
    // RSC rd = (Op2 - Rn + Cy - 1)
    SubtractWithCarryReversed = 0x7,
    // TST void = (Rn AND Op2)
    Test = 0x8,
    // TEQ void = (Rn XOR Op2)
    TestExclusive = 0x9,
    // CMP void = (Rn - Op2)
    Compare = 0xA,
    // CMN void = (Rn + Op2)
    CompareNegative = 0xB,
    // ORR rd = (Rn OR Op2)
    OrLogical = 0xC,
    // MOV rd = Op2
    Move = 0xD,
    // BIC rd = (Rn AND NOT Op2)
    BitClear = 0xE,
    // MVN rd = NOT Op2
    Not = 0xF,
}

impl DataProcessingOpcode {
    pub fn has_rd_dest(&self) -> bool {
        match self {
            DataProcessingOpcode::Test
            | DataProcessingOpcode::TestExclusive
            | DataProcessingOpcode::Compare
            | DataProcessingOpcode::CompareNegative => false,
            _ => true,
        }
    }

    pub fn has_rn_operand(&self) -> bool {
        match self {
            DataProcessingOpcode::Move | DataProcessingOpcode::Not => false,
            _ => true,
        }
    }
}

impl ToString for DataProcessingOpcode {
    fn to_string(&self) -> String {
        match self {
            DataProcessingOpcode::AndLogical => "and",
            DataProcessingOpcode::XorLogical => "eor",
            DataProcessingOpcode::Subtract => "sub",
            DataProcessingOpcode::SubtractReversed => "rsb",
            DataProcessingOpcode::Add => "add",
            DataProcessingOpcode::AddWithCarry => "adc",
            DataProcessingOpcode::SubtractWithCarry => "sbc",
            DataProcessingOpcode::SubtractWithCarryReversed => "rsc",
            DataProcessingOpcode::Test => "tst",
            DataProcessingOpcode::TestExclusive => "teq",
            DataProcessingOpcode::Compare => "cmp",
            DataProcessingOpcode::CompareNegative => "cmn",
            DataProcessingOpcode::OrLogical => "orr",
            DataProcessingOpcode::Move => "mov",
            DataProcessingOpcode::BitClear => "bic",
            DataProcessingOpcode::Not => "mvn",
        }
        .to_string()
    }
}

#[bitfield(u32)]
#[derive(Debug, IntoBytes)]
pub struct Multiply {
    #[bits(0..=3, rw)]
    pub rm_operand1: Register,
    #[bits(4..=7, rw)]
    pub unused: u4,
    #[bits(8..=11, rw)]
    pub rs_operand2: Register,
    // Used as the accumulator register i.e. rd = rm * rs + rn when accumulate is set
    #[bits(12..=15, rw)]
    pub rn_accumulator: Register,
    #[bits(16..=19, rw)]
    pub rd_dest: Register,
    #[bit(20, rw)]
    pub set_condition_codes: bool,
    #[bit(21, rw)]
    pub accumulate: bool,
    #[bits(22..=27, rw)]
    pub unused2: u6,
    #[bits(28..=31, rw)]
    pub condition: Condition,
}

#[bitfield(u32)]
#[derive(Debug, IntoBytes)]
pub struct MultiplyLong {
    #[bits(0..=3, rw)]
    pub rm_operand1: Register,
    #[bits(4..=7, rw)]
    pub unused: u4,
    #[bits(8..=11, rw)]
    pub rs_operand2: Register,
    #[bits(12..=15, rw)]
    pub rdlo_destlo: Register,
    #[bits(16..=19, rw)]
    pub rdhi_desthi: Register,
    #[bit(20, rw)]
    pub set_condition_codes: bool,
    // If set will use rdhi/rdlo as the accumulator registers
    #[bit(21, rw)]
    pub accumulate: bool,
    #[bit(22, rw)]
    pub signed: bool,
    #[bits(23..=27, rw)]
    pub unused2: u5,
    #[bits(28..=31, rw)]
    pub condition: Condition,
}

#[bitenum(u1, exhaustive = true)]
#[derive(Debug, IntoBytes)]
#[repr(u8)]
pub enum DataLength {
    Byte = 1,
    // 32 bit
    Word = 0,
}

impl DataLength {
    pub fn suffix(&self) -> &'static str {
        match self {
            // We don't have an output for word
            DataLength::Byte => "b",
            DataLength::Word => "",
        }
    }
}

// Rd = [Rn], Rn = [Rm]
#[bitfield(u32)]
#[derive(Debug, IntoBytes)]
pub struct SingleDataSwap {
    #[bits(0..=3, rw)]
    pub rm_source: Register,
    #[bits(4..=11, rw)]
    pub unused: u8,
    #[bits(12..=15, rw)]
    pub rd_dest: Register,
    #[bits(16..=19, rw)]
    pub rn_base: Register,
    #[bits(20..=21, rw)]
    pub unused2: u2,
    // Swap bytes or words
    #[bit(22, rw)]
    pub b_length: DataLength,
    #[bits(23..=27, rw)]
    pub unused3: u5,
    #[bits(28..=31, rw)]
    pub condition: Condition,
}

#[bitfield(u32)]
#[derive(Debug, IntoBytes)]
pub struct BranchAndExchange {
    #[bits(0..=3, rw)]
    pub rn_operand: Register,
    #[bits(4..=7, rw)]
    pub opcode: Option<BranchAndExchangeOpcode>,
    #[bits(8..=27, rw)]
    pub unused: u20,
    #[bits(28..=31, rw)]
    pub condition: Condition,
}

#[bitenum(u4, exhaustive = false)]
#[derive(Debug, IntoBytes, PartialEq, Eq)]
#[repr(u8)]
pub enum BranchAndExchangeOpcode {
    BranchAndExchange = 1,     // BX
    BranchAndExchangeLink = 2, // BLX
}

impl BranchAndExchangeOpcode {
    pub fn suffix(&self) -> &'static str {
        match self {
            BranchAndExchangeOpcode::BranchAndExchange => "bx",
            BranchAndExchangeOpcode::BranchAndExchangeLink => "blx",
        }
    }
}

// Is a little confusing because yes if the L = 0 bit is set it means Store
// but we still have "load" instructions.  And realistically we only have 1
// that does really anything (outside of writebacks), which is StoreHalfword.
#[bitenum(u2, exhaustive = true)]
#[derive(Debug, IntoBytes, PartialEq, Eq)]
#[repr(u8)]
pub enum HalfwordOperandStore {
    // Should not be used.
    Reserved = 0,
    StoreHalfword = 1,
    // Doesn't do anything in GBA/ARMv7 but it will still do writebacks
    LoadDoubleWord = 2,
    // Same as LoadDoubleWord
    StoreDoubleWord = 3,
}

impl HalfwordOperandStore {
    pub fn suffix(&self) -> &'static str {
        match self {
            // for SWP
            HalfwordOperandStore::Reserved => "",
            HalfwordOperandStore::StoreHalfword => "h",
            HalfwordOperandStore::LoadDoubleWord | HalfwordOperandStore::StoreDoubleWord => "d",
        }
    }
}

#[bitenum(u2, exhaustive = true)]
#[derive(Debug, IntoBytes)]
#[repr(u8)]
pub enum HalfwordOperandLoad {
    // Should not be used.
    Reserved = 0,
    LoadUnsignedHalfword = 1,
    LoadSignedByte = 2,
    LoadSignedHalfword = 3,
}

impl HalfwordOperandLoad {
    pub fn suffix(&self) -> &'static str {
        match self {
            HalfwordOperandLoad::Reserved => "",
            HalfwordOperandLoad::LoadUnsignedHalfword => "h",
            HalfwordOperandLoad::LoadSignedByte => "sb",
            HalfwordOperandLoad::LoadSignedHalfword => "sh",
        }
    }
}

#[bitenum(u1, exhaustive = true)]
#[derive(Debug, IntoBytes)]
#[repr(u8)]
pub enum OffsetMode {
    SubtractOffsetFromBase = 0, // Down
    AddOffsetToBase = 1,        // Up
}

impl OffsetMode {
    pub fn sign(&self) -> &'static str {
        match self {
            OffsetMode::SubtractOffsetFromBase => "-",
            OffsetMode::AddOffsetToBase => "+",
        }
    }
}

#[bitenum(u1, exhaustive = true)]
#[derive(Debug, IntoBytes, PartialEq, Eq)]
#[repr(u8)]
pub enum IndexingMode {
    // Post (p = 0)
    AddOffsetAfterTransfer = 0,
    // Pre (p = 1)
    AddOffsetBeforeTransfer = 1,
}

#[bitfield(u32)]
#[derive(Debug, IntoBytes)]
pub struct HalfwordDataTransferRegisterStore {
    #[bits(0..=3, rw)]
    pub rm_source: Register,
    #[bit(4, rw)]
    pub unused: bool,
    #[bits(5..=6, rw)]
    pub opcode: HalfwordOperandStore,
    #[bits(7..=11, rw)]
    pub unused_2: u5,
    #[bits(12..=15, rw)]
    pub rd_dest: Register,
    #[bits(16..=19, rw)]
    pub rn_base: Register,
    // Will be 0 in this case since we are a store.
    #[bit(20, rw)]
    pub load: bool,
    // Is ignored in the case that P = 0
    // but the value should be 0 still!
    #[bit(21, rw)]
    pub writeback: bool,
    #[bit(22, rw)]
    pub unused_3: bool,
    #[bit(23, rw)]
    pub offset_mode: OffsetMode,
    #[bit(24, rw)]
    pub indexing: IndexingMode,
    #[bits(25..=27, rw)]
    pub unused_4: u3,
    #[bits(28..=31, rw)]
    pub condition: Condition,
}

impl RegisterInstructionHasAddressingMode for HalfwordDataTransferRegisterStore {
    fn indexing(&self) -> IndexingMode {
        self.indexing()
    }

    fn offset_mode(&self) -> OffsetMode {
        self.offset_mode()
    }

    fn writeback(&self) -> bool {
        self.writeback()
    }

    fn shift_amount(&self) -> u5 {
        u5::new(0) // No shift amount in this case
    }

    fn shift_type(&self) -> ShiftType {
        ShiftType::Lsl // No shift type in this case
    }

    fn rn_base(&self) -> Register {
        self.rn_base()
    }

    fn rm_offset(&self) -> Register {
        self.rm_source()
    }
}

#[bitfield(u32)]
#[derive(Debug, IntoBytes)]
pub struct HalfwordDataTransferRegisterLoad {
    #[bits(0..=3, rw)]
    pub rm_source: Register,
    #[bit(4, rw)]
    pub unused: bool,
    #[bits(5..=6, rw)]
    pub opcode: HalfwordOperandLoad,
    #[bits(7..=11, rw)]
    pub unused_2: u5,
    #[bits(12..=15, rw)]
    pub rd_dest: Register,
    #[bits(16..=19, rw)]
    pub rn_base: Register,
    // Will be 1 in this case since we are a load.
    #[bit(20, rw)]
    pub load: bool,
    // Is ignored in the case that P = 0
    // but the value should be 0 still!
    #[bit(21, rw)]
    pub writeback: bool,
    #[bit(22, rw)]
    pub unused_3: bool,
    #[bit(23, rw)]
    pub offset_mode: OffsetMode,
    #[bit(24, rw)]
    pub indexing: IndexingMode,
    #[bits(25..=27, rw)]
    pub unused_4: u3,
    #[bits(28..=31, rw)]
    pub condition: Condition,
}

impl RegisterInstructionHasAddressingMode for HalfwordDataTransferRegisterLoad {
    fn indexing(&self) -> IndexingMode {
        self.indexing()
    }

    fn offset_mode(&self) -> OffsetMode {
        self.offset_mode()
    }

    fn writeback(&self) -> bool {
        self.writeback()
    }

    fn shift_amount(&self) -> u5 {
        u5::new(0) // No shift amount in this case
    }

    fn shift_type(&self) -> ShiftType {
        ShiftType::Lsl // No shift type in this case
    }

    fn rn_base(&self) -> Register {
        self.rn_base()
    }

    fn rm_offset(&self) -> Register {
        self.rm_source()
    }
}

#[bitfield(u32)]
#[derive(Debug, IntoBytes)]
pub struct HalfwordDataTransferImmediateStore {
    #[bits(0..=3, rw)]
    pub offset: u4,
    #[bit(4, rw)]
    pub unused: bool,
    #[bits(5..=6, rw)]
    pub opcode: HalfwordOperandStore,
    #[bit(7, rw)]
    pub unused_2: bool,
    #[bits(8..=11, rw)]
    pub offset2: u4,
    #[bits(12..=15, rw)]
    pub rd_dest: Register,
    #[bits(16..=19, rw)]
    pub rn_base: Register,
    // Will be 0 in this case since we are a store.
    #[bit(20, rw)]
    pub load: bool,
    // Is ignored in the case that P = 0
    // but the value should be 0 still!
    #[bit(21, rw)]
    pub writeback: bool,
    #[bit(22, rw)]
    pub unused_3: bool,
    #[bit(23, rw)]
    pub offset_mode: OffsetMode,
    #[bit(24, rw)]
    pub indexing: IndexingMode,
    #[bits(25..=27, rw)]
    pub unused_4: u3,
    #[bits(28..=31, rw)]
    pub condition: Condition,
}

impl ImmediateInstructionHasAddressingMode for HalfwordDataTransferImmediateStore {
    fn indexing(&self) -> IndexingMode {
        self.indexing()
    }

    fn offset_mode(&self) -> OffsetMode {
        self.offset_mode()
    }

    fn writeback(&self) -> bool {
        self.writeback()
    }

    fn offset(&self) -> u32 {
        // We have a 4 bit offset, so we can just convert it to u32
        self.offset().as_::<u32>()
    }

    fn rn_base(&self) -> Register {
        self.rn_base()
    }
}

#[bitfield(u32)]
#[derive(Debug, IntoBytes)]
pub struct HalfwordDataTransferImmediateLoad {
    #[bits(0..=3, rw)]
    pub offset: u4,
    #[bit(4, rw)]
    pub unused: bool,
    #[bits(5..=6, rw)]
    pub opcode: HalfwordOperandLoad,
    #[bit(7, rw)]
    pub unused_2: bool,
    #[bits(8..=11, rw)]
    pub offset2: u4,
    #[bits(12..=15, rw)]
    pub rd_dest: Register,
    #[bits(16..=19, rw)]
    pub rn_base: Register,
    // Will be 1 in this case since we are a load.
    #[bit(20, rw)]
    pub load: bool,
    // Is ignored in the case that P = 0
    // but the value should be 0 still!
    #[bit(21, rw)]
    pub writeback: bool,
    #[bit(22, rw)]
    pub unused_3: bool,
    #[bit(23, rw)]
    pub offset_mode: OffsetMode,
    #[bit(24, rw)]
    pub indexing: IndexingMode,
    #[bits(25..=27, rw)]
    pub unused_4: u3,
    #[bits(28..=31, rw)]
    pub condition: Condition,
}

impl ImmediateInstructionHasAddressingMode for HalfwordDataTransferImmediateLoad {
    fn indexing(&self) -> IndexingMode {
        self.indexing()
    }

    fn offset_mode(&self) -> OffsetMode {
        self.offset_mode()
    }

    fn writeback(&self) -> bool {
        self.writeback()
    }

    fn offset(&self) -> u32 {
        // We have a 4 bit offset, so we can just convert it to u32
        self.offset().as_::<u32>()
    }

    fn rn_base(&self) -> Register {
        self.rn_base()
    }
}

#[bitfield(u32)]
#[derive(Debug, IntoBytes)]
pub struct SingleDataTransferImmediate {
    #[bits(0..=11, rw)]
    pub offset: u12,
    #[bits(12..=15, rw)]
    pub rd_dest: Register,
    #[bits(16..=19, rw)]
    pub rn_base: Register,
    #[bit(20, rw)]
    pub load: bool,
    #[bit(21, rw)]
    pub writeback: bool,
    #[bit(22, rw)]
    pub b_length: DataLength,
    #[bit(23, rw)]
    pub offset_mode: OffsetMode,
    #[bit(24, rw)]
    pub indexing: IndexingMode,
    #[bits(25..=27, rw)]
    pub unused: u3,
    #[bits(28..=31, rw)]
    pub condition: Condition,
}

impl ImmediateInstructionHasAddressingMode for SingleDataTransferImmediate {
    fn indexing(&self) -> IndexingMode {
        self.indexing()
    }

    fn offset_mode(&self) -> OffsetMode {
        self.offset_mode()
    }

    fn writeback(&self) -> bool {
        self.writeback()
    }

    fn offset(&self) -> u32 {
        self.offset().as_::<u32>()
    }

    fn rn_base(&self) -> Register {
        self.rn_base()
    }
}

pub trait ImmediateInstructionHasAddressingMode {
    fn indexing(&self) -> IndexingMode;
    fn offset_mode(&self) -> OffsetMode;
    fn writeback(&self) -> bool;
    // Note: you might have a smaller offset such as a u12, but we coalesce all to a u32.
    fn offset(&self) -> u32;
    fn rn_base(&self) -> Register;

    fn addressing_mode(&self) -> String {
        if self.indexing() == IndexingMode::AddOffsetAfterTransfer {
            // Post-indexing
            // [rn], #{+/-}offset
            format!(
                "[{}], #{}{}",
                self.rn_base().to_string(),
                self.offset_mode().sign(),
                self.offset()
            )
        } else if self.offset() == u32::new(0) {
            // special 0 handling
            // [rn]
            format!("[{}]", self.rn_base().to_string())
        } else {
            // Pre-indexing
            // [rn, #{+/-}offset]{!?}
            format!(
                "[{}, #{}{}]{}",
                self.rn_base().to_string(),
                self.offset_mode().sign(),
                self.offset(),
                if self.writeback() { "!" } else { "" }
            )
        }
    }
}

pub trait RegisterInstructionHasAddressingMode {
    fn indexing(&self) -> IndexingMode;
    fn offset_mode(&self) -> OffsetMode;
    fn writeback(&self) -> bool;
    fn shift_amount(&self) -> u5;
    fn shift_type(&self) -> ShiftType;
    fn rn_base(&self) -> Register;
    fn rm_offset(&self) -> Register;

    fn shift(&self) -> String {
        if self.shift_amount() == u5::new(0) {
            String::new()
        } else {
            format!(
                ", {} #{:08x}",
                self.shift_type().suffix(),
                self.shift_amount()
            )
        }
    }

    fn addressing_mode(&self) -> String {
        if self.indexing() == IndexingMode::AddOffsetAfterTransfer {
            // Post-indexing
            // [rn], {+/-}rm{, shift}
            format!(
                "[{}], {}{}{}",
                self.rn_base().to_string(),
                self.offset_mode().sign(),
                self.rm_offset().to_string(),
                self.shift()
            )
        } else {
            // Pre-indexing
            // [rn, {+/-}rm{, shift}]{!}
            format!(
                "[{}, {}{}{}]{}",
                self.rn_base().to_string(),
                self.offset_mode().sign(),
                self.rm_offset().to_string(),
                self.shift(),
                if self.writeback() { "!" } else { "" }
            )
        }
    }
}

#[bitenum(u2, exhaustive = true)]
#[derive(Debug, IntoBytes)]
#[repr(u8)]
pub enum ShiftType {
    Lsl = 0, // Logical Shift Left
    Lsr = 1, // Logical Shift Right
    Asr = 2, // Arithmetic Shift Right
    Ror = 3, // Rotate Right
}

impl ShiftType {
    pub fn suffix(&self) -> &'static str {
        match self {
            ShiftType::Lsl => "lsl",
            ShiftType::Lsr => "lsr",
            ShiftType::Asr => "asr",
            ShiftType::Ror => "ror",
        }
    }
}

#[bitfield(u32)]
#[derive(Debug, IntoBytes)]
pub struct SingleDataTransferShiftedRegister {
    #[bits(0..=3, rw)]
    pub rm_offset: Register,
    #[bit(4, rw)]
    pub unused: bool,
    #[bits(5..=6, rw)]
    pub shift_type: ShiftType,
    /*
      1-31, 0 means
    LSL#0: No shift performed, ie. directly Op2=Rm, the C flag is NOT affected.
    LSR#0: Interpreted as LSR#32, ie. Op2 becomes zero, C becomes Bit 31 of Rm.
    ASR#0: Interpreted as ASR#32, ie. Op2 and C are filled by Bit 31 of Rm.
    ROR#0: Interpreted as RRX#1 (RCR), like ROR#1, but Op2 Bit 31 set to old C.
       */
    #[bits(7..=11, rw)]
    pub shift_amount: u5,
    #[bits(12..=15, rw)]
    pub rd_dest: Register,
    #[bits(16..=19, rw)]
    pub rn_base: Register,
    #[bit(20, rw)]
    pub load: bool,
    #[bit(21, rw)]
    pub writeback: bool,
    #[bit(22, rw)]
    pub b_length: DataLength,
    #[bit(23, rw)]
    pub offset_mode: OffsetMode,
    #[bit(24, rw)]
    pub indexing: IndexingMode,
    #[bits(25..=27, rw)]
    pub unused_2: u3,
    #[bits(28..=31, rw)]
    pub condition: Condition,
}

impl RegisterInstructionHasAddressingMode for SingleDataTransferShiftedRegister {
    fn indexing(&self) -> IndexingMode {
        self.indexing()
    }

    fn offset_mode(&self) -> OffsetMode {
        self.offset_mode()
    }

    fn writeback(&self) -> bool {
        self.writeback()
    }

    fn shift_amount(&self) -> u5 {
        self.shift_amount()
    }

    fn shift_type(&self) -> ShiftType {
        self.shift_type()
    }

    fn rn_base(&self) -> Register {
        self.rn_base()
    }

    fn rm_offset(&self) -> Register {
        self.rm_offset()
    }
}

#[bitfield(u32)]
#[derive(Debug, IntoBytes)]
pub struct BlockDataTransfer {
    // TODO: There seems to be some really weird cases around usermode and banked registers
    #[bit(0, rw)]
    pub register_list: [bool; 16],
    #[bits(16..=19, rw)]
    pub rn_base: Register,
    #[bit(20, rw)]
    pub load: bool,
    #[bit(21, rw)]
    pub writeback: bool,
    // S = 0/1, No/Load PSR and force user mode
    #[bit(22, rw)]
    pub usermode: bool,
    #[bit(23, rw)]
    pub offset_mode: OffsetMode,
    #[bit(24, rw)]
    pub indexing: IndexingMode,
    #[bits(25..=27, rw)]
    pub unused: u3,
    #[bits(28..=31, rw)]
    pub condition: Condition,
}

impl BlockDataTransfer {
    pub fn addressing_mode_suffix(&self) -> &'static str {
        match (self.indexing(), self.offset_mode()) {
            (IndexingMode::AddOffsetAfterTransfer, OffsetMode::SubtractOffsetFromBase) => "fa", // da
            (IndexingMode::AddOffsetBeforeTransfer, OffsetMode::SubtractOffsetFromBase) => "ea", // db
            (IndexingMode::AddOffsetAfterTransfer, OffsetMode::AddOffsetToBase) => "fd", // ia
            (IndexingMode::AddOffsetBeforeTransfer, OffsetMode::AddOffsetToBase) => "ed", // ib
        }
    }

    pub fn register_list_string(&self) -> String {
        // This will produce a string like "r0-r3, r5, r7-r15"
        let mut registers = Vec::new();
        let mut range_start = None;

        for i in 0..16 {
            let bit = self.register_list(i);
            if bit {
                if range_start.is_none() {
                    range_start = Some(i);
                }
            } else {
                if let Some(start) = range_start {
                    // If we only have a single register, we just output it
                    if start - i == 1 {
                        registers.push(format!("r{}", start));
                    } else {
                        registers.push(format!("r{}-r{}", start, i - 1));
                    }
                    range_start = None;
                }
            }
        }

        // Handle the last range if it exists
        if let Some(start) = range_start {
            registers.push(format!("r{}", start));
        }

        registers.join(", ")
    }
}

#[bitfield(u32)]
#[derive(Debug, IntoBytes)]
pub struct Branch {
    #[bits(0..=23, rw)]
    pub offset: u24, // 24 bit signed offset
    #[bit(24, rw)]
    pub link: bool, // L = 1 means link, L = 0 means branch
    #[bits(25..=27, rw)]
    pub unused: u3,
    #[bits(28..=31, rw)]
    pub condition: Condition,
}

impl Branch {
    pub fn signed_offset(&self) -> i24 {
        // Sign extend the 24 bit offset
        self.offset().as_::<i24>()
    }
}

// TODO: Maybe we support breakpoints at some point?
// they don't seeem to much more complex!
#[bitfield(u32)]
#[derive(IntoBytes)]
pub struct SoftwareInterrupt {
    // Ignored by processor, but we output it for debugging
    #[bits(0..=23, rw)]
    pub immediate: u24,
    #[bits(24..=27, rw)]
    pub unused: u4, // Unused bits
    #[bits(28..=31, rw)]
    pub condition: Condition,
}

// This is ordered in the same way as the ARMv7-A documentation
#[derive(IntoBytes)]
#[repr(u32)]
pub enum OpCode {
    DataProcessingImmediate(DataProcessingImmediate),
    DataProcessingRegisterShiftedByImmediate(DataProcessingRegisterShiftedByImmediate),
    DataProcessingRegisterShiftedByRegister(DataProcessingRegisterShiftedByRegister),
    StatusTransferToRegister(StatusTransferToRegister),
    StatusTransferFromImmediate(StatusTransferFromImmediate),
    StatusTransferFromRegister(StatusTransferFromRegister),
    Multiply(Multiply),
    MultiplyLong(MultiplyLong),
    SingleDataSwap(SingleDataSwap),
    BranchAndExchange(BranchAndExchange),
    HalfwordDataTransferRegisterStore(HalfwordDataTransferRegisterStore),
    HalfwordDataTransferRegisterLoad(HalfwordDataTransferRegisterLoad),
    HalfwordDataTransferImmediateStore(HalfwordDataTransferImmediateStore),
    HalfwordDataTransferImmediateLoad(HalfwordDataTransferImmediateLoad),
    SingleDataTransferImmediate(SingleDataTransferImmediate),
    SingleDataTransferShiftedRegister(SingleDataTransferShiftedRegister),
    Undefined(Undefined),
    BlockDataTransfer(BlockDataTransfer),
    Branch(Branch),
    // Note: we don't handle Coprocessor instructions
    SoftwareInterrupt(SoftwareInterrupt),
    Invalid(u32),
}

// Up to block!

impl From<u32> for OpCode {
    fn from(value: u32) -> Self {
        // TODO: Would be awesome to write this more efficiently in future
        // since checking every single pattern is a bit wasteful, we can
        // check for specific bits first in a sort of binary tree fashion.
        if BRANCH_AND_EXCHANGE_BIT_PATTERN.is_match(value) {
            OpCode::BranchAndExchange(BranchAndExchange::new_with_raw_value(value))
        } else if BLOCK_DATA_TRANSFER_BIT_PATTERN.is_match(value) {
            OpCode::BlockDataTransfer(BlockDataTransfer::new_with_raw_value(value))
        } else if BRANCH_BIT_PATTERN.is_match(value) {
            OpCode::Branch(Branch::new_with_raw_value(value))
        } else if SOFTWARE_INTERRUPT_BIT_PATTERN.is_match(value) {
            OpCode::SoftwareInterrupt(SoftwareInterrupt::new_with_raw_value(value))
        } else if UNDEFINED_BIT_PATTERN.is_match(value) {
            OpCode::Undefined(Undefined::new_with_raw_value(value))
        } else if SINGLE_DATA_TRANSFER_IMMEDIATE_BIT_PATTERN.is_match(value) {
            OpCode::SingleDataTransferImmediate(SingleDataTransferImmediate::new_with_raw_value(
                value,
            ))
        } else if SINGLE_DATA_TRANSFER_SHIFTED_REGISTER_BIT_PATTERN.is_match(value) {
            OpCode::SingleDataTransferShiftedRegister(
                SingleDataTransferShiftedRegister::new_with_raw_value(value),
            )
        } else if SINGLE_DATA_SWAP_BIT_PATTERN.is_match(value) {
            OpCode::SingleDataSwap(SingleDataSwap::new_with_raw_value(value))
        } else if MULTIPLY_LONG_BIT_PATTERN.is_match(value) {
            OpCode::MultiplyLong(MultiplyLong::new_with_raw_value(value))
        } else if MULTIPLY_BIT_PATTERN.is_match(value) {
            OpCode::Multiply(Multiply::new_with_raw_value(value))
        } else if MULTIPLY_LONG_BIT_PATTERN.is_match(value) {
            OpCode::HalfwordDataTransferRegisterStore(
                HalfwordDataTransferRegisterStore::new_with_raw_value(value),
            )
        } else if HALFWORD_DATA_TRANSFER_REGISTER_STORE_BIT_PATTERN.is_match(value) {
            OpCode::HalfwordDataTransferRegisterStore(
                HalfwordDataTransferRegisterStore::new_with_raw_value(value),
            )
        } else if HALFWORD_DATA_TRANSFER_REGISTER_LOAD_BIT_PATTERN.is_match(value) {
            OpCode::HalfwordDataTransferRegisterLoad(
                HalfwordDataTransferRegisterLoad::new_with_raw_value(value),
            )
        } else if HALFWORD_DATA_TRANSFER_IMMEDIATE_STORE_BIT_PATTERN.is_match(value) {
            OpCode::HalfwordDataTransferImmediateStore(
                HalfwordDataTransferImmediateStore::new_with_raw_value(value),
            )
        } else if HALFWORD_DATA_TRANSFER_IMMEDIATE_LOAD_BIT_PATTERN.is_match(value) {
            OpCode::HalfwordDataTransferImmediateLoad(
                HalfwordDataTransferImmediateLoad::new_with_raw_value(value),
            )
        } else if DATA_PROCESSING_IMMEDIATE_BIT_PATTERN.is_match(value) {
            OpCode::DataProcessingImmediate(DataProcessingImmediate::new_with_raw_value(value))
        } else if DATA_PROCESSING_REGISTER_SHIFTED_BY_IMMEDIATE_BIT_PATTERN.is_match(value) {
            OpCode::DataProcessingRegisterShiftedByImmediate(
                DataProcessingRegisterShiftedByImmediate::new_with_raw_value(value),
            )
        } else if DATA_PROCESSING_REGISTER_SHIFTED_BY_REGISTER_BIT_PATTERN.is_match(value) {
            OpCode::DataProcessingRegisterShiftedByRegister(
                DataProcessingRegisterShiftedByRegister::new_with_raw_value(value),
            )
        } else if STATUS_TRANSFER_TO_REGISTER_BIT_PATTERN.is_match(value) {
            OpCode::StatusTransferToRegister(StatusTransferToRegister::new_with_raw_value(value))
        } else if STATUS_TRANSFER_FROM_IMMEDIATE_BIT_PATTERN.is_match(value) {
            OpCode::StatusTransferFromImmediate(StatusTransferFromImmediate::new_with_raw_value(
                value,
            ))
        } else if STATUS_TRANSFER_FROM_REGISTER_BIT_PATTERN.is_match(value) {
            OpCode::StatusTransferFromRegister(StatusTransferFromRegister::new_with_raw_value(
                value,
            ))
        } else {
            // Note: idk if we need to parse + ignore coprocessor instructions

            // TODO: I doubt that GBA does this.  But it doesn't look like it treats them
            // as undefined instructions.
            OpCode::Invalid(value)
        }
    }
}

impl ToString for OpCode {
    fn to_string(&self) -> String {
        // This writes it out using the ASM syntax
        match self {
            OpCode::DataProcessingImmediate(data_processing_immediate) => format!(
                // {opcode}{cond}{s?} {rd?},{rn?},{op2}
                "{}{}{} {},{},#{}",
                data_processing_immediate.opcode().to_string(),
                data_processing_immediate.condition().suffix(),
                if data_processing_immediate.set_condition_codes() {
                    "s"
                } else {
                    ""
                },
                if data_processing_immediate.opcode().has_rd_dest() {
                    format!(", {}", data_processing_immediate.rd_dest().to_string())
                } else {
                    String::new()
                },
                if data_processing_immediate.opcode().has_rn_operand() {
                    format!(", {}", data_processing_immediate.rn_operand().to_string())
                } else {
                    String::new()
                },
                data_processing_immediate.immediate(),
            ),
            OpCode::DataProcessingRegisterShiftedByRegister(
                data_processing_register_shifted_by_register,
            ) => format!(
                // {opcode}{cond}{s?} {rd?},{rn?},{rm},{shift} {rs}
                "{}{}{} {}{},{},{} {}",
                data_processing_register_shifted_by_register
                    .opcode()
                    .to_string(),
                data_processing_register_shifted_by_register
                    .condition()
                    .suffix(),
                if data_processing_register_shifted_by_register.set_condition_codes() {
                    "s"
                } else {
                    ""
                },
                if data_processing_register_shifted_by_register
                    .opcode()
                    .has_rd_dest()
                {
                    format!(
                        "{}",
                        data_processing_register_shifted_by_register
                            .rd_dest()
                            .to_string()
                    )
                } else {
                    String::new()
                },
                if data_processing_register_shifted_by_register
                    .opcode()
                    .has_rn_operand()
                {
                    format!(
                        "{}{}",
                        if data_processing_register_shifted_by_register
                            .opcode()
                            .has_rd_dest()
                        {
                            ", "
                        } else {
                            ""
                        },
                        data_processing_register_shifted_by_register
                            .rn_operand()
                            .to_string()
                    )
                } else {
                    String::new()
                },
                data_processing_register_shifted_by_register
                    .rm_operand2()
                    .to_string(),
                data_processing_register_shifted_by_register
                    .shift_type()
                    .suffix(),
                data_processing_register_shifted_by_register
                    .rs_shift()
                    .to_string()
            ),
            OpCode::StatusTransferToRegister(status_transfer_to_register) => format!(
                // mrs{cond} {rd},{psr}
                "mrs{} {},{}",
                status_transfer_to_register.condition().suffix(),
                status_transfer_to_register.rd_dest().to_string(),
                status_transfer_to_register.psr().suffix()
            ),
            OpCode::StatusTransferFromImmediate(status_transfer_from_immediate) => format!(
                // msr{cond} {psr}{_field},#{immediate}
                "msr{} {},#{}",
                status_transfer_from_immediate.condition().suffix(),
                status_transfer_from_immediate.fields().to_string(),
                status_transfer_from_immediate.immediate()
            ),
            OpCode::StatusTransferFromRegister(status_transfer_from_register) => format!(
                // msr{cond} {psr}{_field},{rm}
                "msr{} {},{}",
                status_transfer_from_register.condition().suffix(),
                status_transfer_from_register.fields().to_string(),
                status_transfer_from_register.rm_source().to_string()
            ),
            OpCode::DataProcessingRegisterShiftedByImmediate(
                data_processing_register_shifted_by_immediate,
            ) => format!(
                // {opcode}{cond}{s?} {rd?},{rn?},{rm}{, shift}
                "{}{}{} {}{},{}{}",
                data_processing_register_shifted_by_immediate
                    .opcode()
                    .to_string(),
                data_processing_register_shifted_by_immediate
                    .condition()
                    .suffix(),
                if data_processing_register_shifted_by_immediate.set_condition_codes() {
                    "s"
                } else {
                    ""
                },
                if data_processing_register_shifted_by_immediate
                    .opcode()
                    .has_rd_dest()
                {
                    format!(
                        "{}",
                        data_processing_register_shifted_by_immediate
                            .rd_dest()
                            .to_string()
                    )
                } else {
                    String::new()
                },
                if data_processing_register_shifted_by_immediate
                    .opcode()
                    .has_rn_operand()
                {
                    format!(
                        "{}{}",
                        if data_processing_register_shifted_by_immediate
                            .opcode()
                            .has_rd_dest()
                        {
                            ", "
                        } else {
                            ""
                        },
                        data_processing_register_shifted_by_immediate
                            .rn_operand()
                            .to_string()
                    )
                } else {
                    String::new()
                },
                data_processing_register_shifted_by_immediate
                    .rm_operand2()
                    .to_string(),
                data_processing_register_shifted_by_immediate.get_shift_string()
            ),
            OpCode::Multiply(multiply) => format!(
                // {mul/mla}{cond}{s?} {rd},{rm},{rs}{, rn}
                "{}{}{} {},{},{}{}",
                if multiply.accumulate() { "mla" } else { "mul" },
                multiply.condition().suffix(),
                if multiply.set_condition_codes() {
                    "s"
                } else {
                    ""
                },
                multiply.rd_dest().to_string(),
                multiply.rm_operand1().to_string(),
                multiply.rs_operand2().to_string(),
                if multiply.accumulate() {
                    format!(",{}", multiply.rn_accumulator().to_string())
                } else {
                    String::new()
                }
            ),
            OpCode::MultiplyLong(multiply_long) => format!(
                // {s/u}{mull/mlal}{cond}{s?} {rdlo},{rdhi},{rm},{rs}
                "{}{}{} {},{},{},{}",
                if multiply_long.signed() { "s" } else { "u" },
                if multiply_long.accumulate() {
                    "mlal"
                } else {
                    "mull"
                },
                multiply_long.condition().suffix(),
                multiply_long.rdlo_destlo().to_string(),
                multiply_long.rdhi_desthi().to_string(),
                multiply_long.rm_operand1().to_string(),
                multiply_long.rs_operand2().to_string()
            ),
            OpCode::SingleDataSwap(single_data_swap) => format!(
                // swp{cond}{B?} {rd},{rm},[{rn}]
                "swp{}{} {},{},[{}]",
                single_data_swap.condition().suffix(),
                single_data_swap.b_length().suffix(),
                single_data_swap.rd_dest().to_string(),
                single_data_swap.rm_source().to_string(),
                single_data_swap.rn_base().to_string()
            ),
            OpCode::BranchAndExchange(branch_and_exchange) => format!(
                // {bx/blx}{cond} {rn}
                "{}{} {}",
                branch_and_exchange
                    .opcode()
                    .map(|opcode| opcode.suffix())
                    .unwrap_or(""),
                branch_and_exchange.condition().suffix(),
                branch_and_exchange.rn_operand().to_string()
            ),
            OpCode::HalfwordDataTransferRegisterStore(halfword_data_transfer_register_store) => {
                format!(
                    // we are always a store
                    // {str/ldr}{cond}{opcode} {rd},{address}
                    "{}{}{} {},{}",
                    if halfword_data_transfer_register_store.opcode()
                        == HalfwordOperandStore::LoadDoubleWord
                    {
                        "ldr"
                    } else {
                        "str"
                    },
                    halfword_data_transfer_register_store.condition().suffix(),
                    halfword_data_transfer_register_store.opcode().suffix(),
                    halfword_data_transfer_register_store.rd_dest().to_string(),
                    halfword_data_transfer_register_store.addressing_mode(),
                )
            }
            OpCode::HalfwordDataTransferRegisterLoad(halfword_data_transfer_register_load) => {
                format!(
                    // we are always a load
                    // ldr{cond}{opcode} {rd},{address}
                    "ldr{}{} {},{}",
                    halfword_data_transfer_register_load.condition().suffix(),
                    halfword_data_transfer_register_load.opcode().suffix(),
                    halfword_data_transfer_register_load.rd_dest().to_string(),
                    halfword_data_transfer_register_load.addressing_mode(),
                )
            }
            OpCode::HalfwordDataTransferImmediateStore(halfword_data_transfer_immediate_store) => {
                format!(
                    // we are always a store
                    // {str/ldr}{cond}{opcode} {rd},{address}
                    "{}{}{} {},{}",
                    if halfword_data_transfer_immediate_store.opcode()
                        == HalfwordOperandStore::LoadDoubleWord
                    {
                        "ldr"
                    } else {
                        "str"
                    },
                    halfword_data_transfer_immediate_store.condition().suffix(),
                    halfword_data_transfer_immediate_store.opcode().suffix(),
                    halfword_data_transfer_immediate_store.rd_dest().to_string(),
                    halfword_data_transfer_immediate_store.addressing_mode(),
                )
            }
            OpCode::HalfwordDataTransferImmediateLoad(halfword_data_transfer_immediate_load) => {
                format!(
                    // we are always a load
                    // ldr{cond}{opcode} {rd},{address}
                    "ldr{}{} {},{}",
                    halfword_data_transfer_immediate_load.condition().suffix(),
                    halfword_data_transfer_immediate_load.opcode().suffix(),
                    halfword_data_transfer_immediate_load.rd_dest().to_string(),
                    halfword_data_transfer_immediate_load.addressing_mode(),
                )
            }
            OpCode::SingleDataTransferImmediate(single_data_transfer_immediate) => format!(
                // {ldr/str}{cond}{B?}{T?}{rd}{addressing_mode}
                "{}{}{}{} {},{}",
                if single_data_transfer_immediate.load() {
                    "ldr"
                } else {
                    "str"
                },
                single_data_transfer_immediate.condition().suffix(),
                single_data_transfer_immediate.b_length().suffix(),
                // In this specific case only we output our memory management bit!
                // TODO: Is this even needed??
                if single_data_transfer_immediate.indexing() == IndexingMode::AddOffsetAfterTransfer
                    && single_data_transfer_immediate.writeback()
                {
                    "T"
                } else {
                    ""
                },
                single_data_transfer_immediate.rd_dest().to_string(),
                single_data_transfer_immediate.addressing_mode(),
            ),
            OpCode::SingleDataTransferShiftedRegister(single_data_transfer_shifted_register) => {
                format!(
                    "{}{}{}{} {},{}",
                    if single_data_transfer_shifted_register.load() {
                        "ldr"
                    } else {
                        "str"
                    },
                    single_data_transfer_shifted_register.condition().suffix(),
                    single_data_transfer_shifted_register.b_length().suffix(),
                    // In this specific case only we output our memory management bit!
                    // TODO: Is this even needed??
                    if single_data_transfer_shifted_register.indexing()
                        == IndexingMode::AddOffsetAfterTransfer
                        && single_data_transfer_shifted_register.writeback()
                    {
                        "T"
                    } else {
                        ""
                    },
                    single_data_transfer_shifted_register.rd_dest().to_string(),
                    single_data_transfer_shifted_register.addressing_mode(),
                )
            }
            // Maybe there is some way to actually encode this?
            OpCode::Undefined(undefined) => format!(
                "@ Undefined Instruction, raw hex is 0x{:08X}",
                undefined.raw_value()
            ),
            OpCode::BlockDataTransfer(block_data_transfer) => format!(
                "{}{}{} {}{}, {}{}",
                if block_data_transfer.load() {
                    "ldm"
                } else {
                    "stm"
                },
                block_data_transfer.condition().suffix(),
                block_data_transfer.addressing_mode_suffix(),
                block_data_transfer.rn_base().to_string(),
                if block_data_transfer.writeback() {
                    "!"
                } else {
                    ""
                },
                block_data_transfer.register_list_string(),
                if block_data_transfer.usermode() {
                    "^"
                } else {
                    ""
                }
            ),
            OpCode::Branch(branch) => {
                // Branch instructions are a bit special since they have a signed offset
                // so we need to format it differently.
                let offset = branch.offset();
                // TODO: We should produce labels here :)  But that is more complex, so I'm skipping it for now.
                if branch.link() {
                    format!("bl{} #0x{:08X}", branch.condition().suffix(), offset)
                } else {
                    format!("b{} #0x{:08X}", branch.condition().suffix(), offset)
                }
            }
            OpCode::SoftwareInterrupt(software_interrupt) => {
                format!("swi 0x{:06X}", software_interrupt.immediate())
            }
            // For invalid opcodes we will just output a comment!
            OpCode::Invalid(v) => {
                format!("@ Invalid Opcode, raw hex is 0x{:08X}", v)
            }
        }
    }
}
