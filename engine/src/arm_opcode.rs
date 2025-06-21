use arbitrary_int::prelude::*;
use bitbybit::{bitenum, bitfield};
use bitpatterns::{BitPattern, bitpattern};
use zerocopy_derive::IntoBytes;

use crate::state::Register;

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

const DATA_PROCESSING_BIT_PATTERN: BitPattern<u32> =
    bitpattern!("0b...._001._...._...._...._...._...._....");
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

#[bitfield(u32)]
#[derive(Debug, IntoBytes)]
pub struct DataProcessing {
    #[bits(0..=11, rw)]
    pub operand2: u12,
    #[bits(12..=15, rw)]
    pub rd_dest: Register,
    #[bits(16..=19, rw)]
    pub rn_operand: Register,
    #[bit(20, rw)]
    pub set_condition_codes: bool,
    #[bits(21..=24, rw)]
    pub opcode: u4,
    #[bits(25..=27, rw)]
    pub unused: u3,
    #[bits(28..=31, rw)]
    pub condition: Condition,
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
    pub rn_operand2: Register,
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
    #[bits(4..=27, rw)]
    pub unused: u24,
    #[bits(28..=31, rw)]
    pub condition: Condition,
}

// Is a little confusing because yes if the L = 0 bit is set it means Store
// but we still have "load" instructions.  And realistically we only have 1
// that does really anything (outside of writebacks), which is StoreHalfword.
#[bitenum(u2, exhaustive = true)]
#[derive(Debug, IntoBytes)]
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

impl SingleDataTransferImmediate {
    pub fn addressing_mode(&self) -> String {
        if self.indexing() == IndexingMode::AddOffsetAfterTransfer {
            format!(
                "[{}], #{}{}",
                self.rn_base().to_string(),
                self.offset_mode().sign(),
                self.offset()
            )
        } else if self.offset() == u12::new(0) {
            format!("[{}]", self.rn_base().to_string())
        } else {
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

impl SingleDataTransferShiftedRegister {
    pub fn shift(&self) -> String {
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

    pub fn addressing_mode(&self) -> String {
        if self.indexing() == IndexingMode::AddOffsetAfterTransfer {
            format!(
                "[{}], {}{}{}",
                self.rn_base().to_string(),
                self.offset_mode().sign(),
                self.rm_offset().to_string(),
                self.shift()
            )
        } else {
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
    DataProcessing(DataProcessing),
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
        // TODO: PSR Transfer? MRS vs MSR?, DataProcessing Register?
        } else if DATA_PROCESSING_BIT_PATTERN.is_match(value) {
            OpCode::DataProcessing(DataProcessing::new_with_raw_value(value))
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
            OpCode::DataProcessing(data_processing) => todo!(),
            OpCode::Multiply(multiply) => todo!(),
            OpCode::MultiplyLong(multiply_long) => todo!(),
            OpCode::SingleDataSwap(single_data_swap) => todo!(),
            OpCode::BranchAndExchange(branch_and_exchange) => todo!(),
            OpCode::HalfwordDataTransferRegisterStore(halfword_data_transfer_register_store) => {
                todo!()
            }
            OpCode::HalfwordDataTransferRegisterLoad(halfword_data_transfer_register_load) => {
                todo!()
            }
            OpCode::HalfwordDataTransferImmediateStore(halfword_data_transfer_immediate_store) => {
                todo!()
            }
            OpCode::HalfwordDataTransferImmediateLoad(halfword_data_transfer_immediate_load) => {
                todo!()
            }
            OpCode::SingleDataTransferImmediate(single_data_transfer_immediate) => format!(
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
