use bitbybit::{bitenum, bitfield};
use bitpatterns::{BitPattern, bitpattern};

#[derive(Debug)]
#[bitenum(u4, exhaustive = true)]
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
pub struct Undefined {
    #[bits(0..=27)]
    pub unused: u28, // All bits are unused in this case
    #[bits(28..=31)]
    pub condition: Condition,
}

#[bitfield(u32)]
pub struct DataProcessing {
    #[bits(0..=11)]
    pub operand2: u12,
    #[bits(12..=15)]
    pub rd_dest: Register,
    #[bits(16..=19)]
    pub rn_operand: Register,
    #[bit(20)]
    pub set_condition_codes: bool,
    #[bits(21..=24)]
    pub opcode: u4,
    #[bits(25..=27)]
    pub unused: u3,
    #[bits(28..=31)]
    pub condition: Condition,
}

#[bitfield(u32)]
pub struct Multiply {
    #[bits(0..=3)]
    pub rm_operand1: Register,
    #[bits(4..=7)]
    pub unused: u4,
    #[bits(8..=11)]
    pub rs_operand2: Register,
    // Used as the accumulator register i.e. rd = rm * rs + rn when accumulate is set
    #[bits(12..=15)]
    pub rn_accumulator: Register,
    #[bits(16..=19)]
    pub rd_dest: Register,
    #[bit(20)]
    pub setConditionCodes: bool,
    #[bit(21)]
    pub accumulate: bool,
    #[bits(22..=27)]
    pub unused2: u6,
    #[bits(28..=31)]
    pub condition: Condition,
}

#[bitfield(u32)]
pub struct MultiplyLong {
    #[bits(0..=3)]
    pub rm_operand1: Register,
    #[bits(4..=7)]
    pub unused: u4,
    #[bits(8..=11)]
    pub rn_operand2: Register,
    #[bits(12..=15)]
    pub rdlo_destlo: Register,
    #[bits(16..=19)]
    pub rdhi_desthi: Register,
    #[bit(20)]
    pub setConditionCodes: bool,
    // If set will use rdhi/rdlo as the accumulator registers
    #[bit(21)]
    pub accumulate: bool,
    #[bit(22)]
    pub signed: bool,
    #[bits(23..=27)]
    pub unused2: u5,
    #[bits(28..=31)]
    pub condition: Condition,
}

#[bitenum(u1, exhaustive = true)]
pub enum DataLength {
    Byte = 1,
    // 32 bit
    Word = 0,
}

// Rd = [Rn], Rn = [Rm]
#[bitfield(u32)]
pub struct SingleDataSwap {
    #[bits(0..=3)]
    pub rm_source: Register,
    #[bits(4..=11)]
    pub unused: u8,
    #[bits(12..=15)]
    pub rd_dest: Register,
    #[bits(16..=19)]
    pub rn_base: Register,
    #[bits(20..=21)]
    pub unused2: u2,
    // Swap bytes or words
    #[bit(22)]
    pub b_length: DataLength,
    #[bits(23..=27)]
    pub unused2: u5,
    #[bits(28..=31)]
    pub condition: Condition,
}

#[bitfield(u32)]
pub struct BranchAndExchange {
    #[bits(0..=3)]
    pub rn_operand: Register,
    #[bits(4..=27)]
    pub unused: u24,
    #[bits(28..=31)]
    pub condition: Condition,
}

// Is a little confusing because yes if the L = 0 bit is set it means Store
// but we still have "load" instructions.  And realistically we only have 1
// that does really anything (outside of writebacks), which is StoreHalfword.
#[bitenum(u2, exhaustive = true)]
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
pub enum HalfwordOperandLoad {
    // Should not be used.
    Reserved = 0,
    LoadUnsignedHalfword = 1,
    LoadSignedByte = 2,
    LoadSignedHalfword = 3,
}

#[bitenum(u1, exhaustive = true)]
pub enum OffsetMode {
    SubtractOffsetFromBase = 0, // Down
    AddOffsetToBase = 1,        // Up
}

#[bitenum(u1, exhaustive = true)]
pub enum IndexingMode {
    AddOffsetAfterTransfer = 0,  // Post
    AddOffsetBeforeTransfer = 1, // Pre
}

#[bitfield(u32)]
pub struct HalfwordDataTransferRegisterStore {
    #[bits(0..=3)]
    pub rm_source: Register,
    #[bit(4)]
    pub unused: bool,
    #[bits(5..=6)]
    pub opcode: HalfwordOperandStore,
    #[bits(7..=11)]
    pub unused_2: u5,
    #[bits(12..=15)]
    pub rd_dest: Register,
    #[bits(16..=19)]
    pub rn_base: Register,
    // Will be 0 in this case since we are a store.
    #[bit(20)]
    pub load: bool,
    // Is ignored in the case that P = 0
    // but the value should be 0 still!
    #[bit(21)]
    pub writeback: bool,
    #[bit(22)]
    pub unused_3: bool,
    #[bit(23)]
    pub offsetMode: OffsetMode,
    #[bit(24)]
    pub indexing: IndexingMode,
    #[bits(25..=27)]
    pub unused_3: u3,
    #[bits(28..=31)]
    pub condition: Condition,
}

#[bitfield(u32)]
pub struct HalfwordDataTransferRegisterLoad {
    #[bits(0..=3)]
    pub rm_source: Register,
    #[bit(4)]
    pub unused: bool,
    #[bits(5..=6)]
    pub opcode: HalfwordOperandLoad,
    #[bits(7..=11)]
    pub unused_2: u5,
    #[bits(12..=15)]
    pub rd_dest: Register,
    #[bits(16..=19)]
    pub rn_base: Register,
    // Will be 1 in this case since we are a load.
    #[bit(20)]
    pub load: bool,
    // Is ignored in the case that P = 0
    // but the value should be 0 still!
    #[bit(21)]
    pub writeback: bool,
    #[bit(22)]
    pub unused_3: bool,
    #[bit(23)]
    pub offsetMode: OffsetMode,
    #[bit(24)]
    pub indexing: IndexingMode,
    #[bits(25..=27)]
    pub unused_3: u3,
    #[bits(28..=31)]
    pub condition: Condition,
}

#[bitfield(u32)]
pub struct HalfwordDataTransferImmediateStore {
    #[bits(0..=3)]
    pub offset: u4,
    #[bit(4)]
    pub unused: bool,
    #[bits(5..=6)]
    pub opcode: HalfwordOperandStore,
    #[bit(7)]
    pub unused_2: bool,
    #[bits(8..=11)]
    pub offset2: u4,
    #[bits(12..=15)]
    pub rd_dest: Register,
    #[bits(16..=19)]
    pub rn_base: Register,
    // Will be 0 in this case since we are a store.
    #[bit(20)]
    pub load: bool,
    // Is ignored in the case that P = 0
    // but the value should be 0 still!
    #[bit(21)]
    pub writeback: bool,
    #[bit(22)]
    pub unused_3: bool,
    #[bit(23)]
    pub offsetMode: OffsetMode,
    #[bit(24)]
    pub indexing: IndexingMode,
    #[bits(25..=27)]
    pub unused_4: u3,
    #[bits(28..=31)]
    pub condition: Condition,
}

#[bitfield(u32)]
pub struct HalfwordDataTransferImmediateLoad {
    #[bits(0..=3)]
    pub offset: u4,
    #[bit(4)]
    pub unused: bool,
    #[bits(5..=6)]
    pub opcode: HalfwordOperandLoad,
    #[bit(7)]
    pub unused_2: bool,
    #[bits(8..=11)]
    pub offset2: u4,
    #[bits(12..=15)]
    pub rd_dest: Register,
    #[bits(16..=19)]
    pub rn_base: Register,
    // Will be 1 in this case since we are a load.
    #[bit(20)]
    pub load: bool,
    // Is ignored in the case that P = 0
    // but the value should be 0 still!
    #[bit(21)]
    pub writeback: bool,
    #[bit(22)]
    pub unused_3: bool,
    #[bit(23)]
    pub offsetMode: OffsetMode,
    #[bit(24)]
    pub indexing: IndexingMode,
    #[bits(25..=27)]
    pub unused_4: u3,
    #[bits(28..=31)]
    pub condition: Condition,
}

#[bitfield(u32)]
pub struct SingleDataTransferImmediate {
    #[bits(0..=11)]
    pub offset: u12,
    #[bits(12..=15)]
    pub rd_dest: Register,
    #[bits(16..=19)]
    pub rn_base: Register,
    #[bit(20)]
    pub load: bool,
    #[bit(21)]
    pub writeback: bool,
    #[bit(22)]
    pub b_length: DataLength,
    #[bit(23)]
    pub offsetMode: OffsetMode,
    #[bit(24)]
    pub indexing: IndexingMode,
    #[bits(25..=27)]
    pub unused: u3,
    #[bits(28..=31)]
    pub condition: Condition,
}

#[bitenum(u2, exhaustive = true)]
pub enum ShiftType {
    Lsl = 0, // Logical Shift Left
    Lsr = 1, // Logical Shift Right
    Asr = 2, // Arithmetic Shift Right
    Ror = 3, // Rotate Right
}

#[bitfield(u32)]
pub struct SingleDataTransferShiftedRegister {
    #[bits(0..=3)]
    pub rm_offset: Register,
    #[bit(4)]
    pub unused: bool,
    #[bits(5..=6)]
    pub shift_type: ShiftType,
    /*
      1-31, 0 means
    LSL#0: No shift performed, ie. directly Op2=Rm, the C flag is NOT affected.
    LSR#0: Interpreted as LSR#32, ie. Op2 becomes zero, C becomes Bit 31 of Rm.
    ASR#0: Interpreted as ASR#32, ie. Op2 and C are filled by Bit 31 of Rm.
    ROR#0: Interpreted as RRX#1 (RCR), like ROR#1, but Op2 Bit 31 set to old C.
       */
    #[bits(7..=11)]
    pub shift_amount: u5,
    #[bits(12..=15)]
    pub rd_dest: Register,
    #[bits(16..=19)]
    pub rn_base: Register,
    #[bit(20)]
    pub load: bool,
    #[bit(21)]
    pub writeback: bool,
    #[bit(22)]
    pub b_length: DataLength,
    #[bit(23)]
    pub offsetMode: OffsetMode,
    #[bit(24)]
    pub indexing: IndexingMode,
    #[bits(25..=27)]
    pub unused_2: u3,
    #[bits(28..=31)]
    pub condition: Condition,
}

#[bitfield(u32)]
pub struct BlockDataTransfer {
    // TODO: There seems to be some really weird cases around usermode and banked registers
    #[bits(0..=15)]
    pub register_list: u16,
    #[bits(16..=19)]
    pub rn_base: Register,
    #[bit(20)]
    pub load: bool,
    #[bit(21)]
    pub writeback: bool,
    // S = 0/1, No/Load PSR and force user mode
    #[bit(22)]
    pub usermode: bool,
    #[bit(23)]
    pub offsetMode: OffsetMode,
    #[bit(24)]
    pub indexing: IndexingMode,
    #[bits(25..=27)]
    pub unused: u3,
    #[bits(28..=31)]
    pub condition: Condition,
}

#[bitfield(u32)]
pub struct Branch {
    #[bits(0..=23)]
    pub offset: s24, // 24 bit signed offset
    #[bit(24)]
    pub link: bool, // L = 1 means link, L = 0 means branch
    #[bits(25..=27)]
    pub unused: u3,
    #[bits(28..=31)]
    pub condition: Condition,
}

// TODO: Maybe we support breakpoints at some point?
// they don't seeem to much more complex!
#[bitfield(u32)]
pub struct SoftwareInterrupt {
    #[bits(0..=27)]
    pub unused: u28,
    #[bits(28..=31)]
    pub condition: Condition,
}

// This is ordered in the same way as the ARMv7-A documentation
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
    Invalid,
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
            OpCode::Invalid
        }
    }
}
