use std::collections::HashMap;

use crate::cpu::AddressingMode;

pub struct OpCode {
    pub code: u8,
    pub mnemonic: Mnemonic,
    pub mode: AddressingMode,
}

impl OpCode {
    fn new(code: u8, mnemonic: Mnemonic, mode: AddressingMode) -> Self {
        OpCode {
            code,
            mnemonic,
            mode,
        }
    }
    pub fn pc_offset(&self) -> u8 {
        match self.mode {
            AddressingMode::NoneAddressing => 0,
            AddressingMode::Immediate => 1,
            AddressingMode::ZeroPage => 1,
            AddressingMode::ZeroPage_X => 1,
            AddressingMode::ZeroPage_Y => 1,
            AddressingMode::Absolute => 2,
            AddressingMode::Absolute_X => 2,
            AddressingMode::Absolute_Y => 2,
            AddressingMode::Indirect_X => 1,
            AddressingMode::Indirect_Y => 1,
        }
    }
}

pub enum Mnemonic {
    BRK,
    LDA,
    LDX,
    LDY,
    STA,
    STX,
    STY,
    TAX,
    TAY,
    TSX,
    TXA,
    TXS,
    TYA,
    ADC,
    SBC,
}

lazy_static! {
    pub static ref CPU_OPS_CODES: Vec<OpCode> = vec![
        // BRK
        OpCode::new(0x00, Mnemonic::BRK, AddressingMode::NoneAddressing),

        // LDA
        OpCode::new(0xa9, Mnemonic::LDA, AddressingMode::Immediate),
        OpCode::new(0xa5, Mnemonic::LDA, AddressingMode::ZeroPage),
        OpCode::new(0xb5, Mnemonic::LDA, AddressingMode::ZeroPage_X),
        OpCode::new(0xad, Mnemonic::LDA, AddressingMode::Absolute),
        OpCode::new(0xbd, Mnemonic::LDA, AddressingMode::Absolute_X),
        OpCode::new(0xb9, Mnemonic::LDA, AddressingMode::Absolute_Y),
        OpCode::new(0xa1, Mnemonic::LDA, AddressingMode::Indirect_X),
        OpCode::new(0xb1, Mnemonic::LDA, AddressingMode::Indirect_Y),

        // LDX
        OpCode::new(0xa2, Mnemonic::LDX, AddressingMode::Immediate),
        OpCode::new(0xa6, Mnemonic::LDX, AddressingMode::ZeroPage),
        OpCode::new(0xb6, Mnemonic::LDX, AddressingMode::ZeroPage_Y),
        OpCode::new(0xae, Mnemonic::LDX, AddressingMode::Absolute),
        OpCode::new(0xbe, Mnemonic::LDX, AddressingMode::Absolute_Y),

        // LDY
        OpCode::new(0xa0, Mnemonic::LDY, AddressingMode::Immediate),
        OpCode::new(0xa4, Mnemonic::LDY, AddressingMode::ZeroPage),
        OpCode::new(0xb4, Mnemonic::LDY, AddressingMode::ZeroPage_X),
        OpCode::new(0xac, Mnemonic::LDY, AddressingMode::Absolute),
        OpCode::new(0xbc, Mnemonic::LDY, AddressingMode::Absolute_X),

        // STA
        OpCode::new(0x85, Mnemonic::STA, AddressingMode::ZeroPage),
        OpCode::new(0x95, Mnemonic::STA, AddressingMode::ZeroPage_X),
        OpCode::new(0x8d, Mnemonic::STA, AddressingMode::Absolute),
        OpCode::new(0x9d, Mnemonic::STA, AddressingMode::Absolute_X),
        OpCode::new(0x99, Mnemonic::STA, AddressingMode::Absolute_Y),
        OpCode::new(0x81, Mnemonic::STA, AddressingMode::Indirect_X),
        OpCode::new(0x91, Mnemonic::STA, AddressingMode::Indirect_Y),

        // STX
        OpCode::new(0x86, Mnemonic::STX, AddressingMode::ZeroPage),
        OpCode::new(0x96, Mnemonic::STX, AddressingMode::ZeroPage_Y),
        OpCode::new(0x8e, Mnemonic::STX, AddressingMode::Absolute),

        // STY
        OpCode::new(0x84, Mnemonic::STY, AddressingMode::ZeroPage),
        OpCode::new(0x94, Mnemonic::STY, AddressingMode::ZeroPage_X),
        OpCode::new(0x8c, Mnemonic::STY, AddressingMode::Absolute),

        // TAX
        OpCode::new(0xaa, Mnemonic::TAX, AddressingMode::NoneAddressing),

        // TAY
        OpCode::new(0xa8, Mnemonic::TAY, AddressingMode::NoneAddressing),

        // TSX
        OpCode::new(0xba, Mnemonic::TSX, AddressingMode::NoneAddressing),

        // TXA
        OpCode::new(0x8a, Mnemonic::TXA, AddressingMode::NoneAddressing),

        // TXS
        OpCode::new(0x9a, Mnemonic::TXS, AddressingMode::NoneAddressing),

        // TYA
        OpCode::new(0x98, Mnemonic::TYA, AddressingMode::NoneAddressing),

        // ADC
        OpCode::new(0x69, Mnemonic::ADC, AddressingMode::Immediate),
        OpCode::new(0x65, Mnemonic::ADC, AddressingMode::ZeroPage),
        OpCode::new(0x75, Mnemonic::ADC, AddressingMode::ZeroPage_X),
        OpCode::new(0x6d, Mnemonic::ADC, AddressingMode::Absolute),
        OpCode::new(0x7d, Mnemonic::ADC, AddressingMode::Absolute_X),
        OpCode::new(0x79, Mnemonic::ADC, AddressingMode::Absolute_Y),
        OpCode::new(0x61, Mnemonic::ADC, AddressingMode::Indirect_X),
        OpCode::new(0x71, Mnemonic::ADC, AddressingMode::Indirect_Y),

        // SBC
        OpCode::new(0xe9, Mnemonic::SBC, AddressingMode::Immediate),
    ];

    pub static ref OPCODES_MAP: HashMap<u8, &'static OpCode> = {
        let mut map = HashMap::new();
        for cpuop in &*CPU_OPS_CODES {
            map.insert(cpuop.code, cpuop);
        }
        map
    };
}