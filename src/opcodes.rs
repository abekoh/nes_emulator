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
    AND,
    ORA,
    EOR,
    BIT,
    ASL,
    LSR,
    ROL,
    ROR,
    CMP,
    CPX,
    CPY,
    DEC,
    INC,
    DEX,
    DEY,
    INX,
    INY,
    PHA,
    PHP,
    PLA,
    BRK,
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
        OpCode::new(0xe5, Mnemonic::SBC, AddressingMode::ZeroPage),
        OpCode::new(0xf5, Mnemonic::SBC, AddressingMode::ZeroPage_X),
        OpCode::new(0xed, Mnemonic::SBC, AddressingMode::Absolute),
        OpCode::new(0xfd, Mnemonic::SBC, AddressingMode::Absolute_X),
        OpCode::new(0xf9, Mnemonic::SBC, AddressingMode::Absolute_Y),
        OpCode::new(0xe1, Mnemonic::SBC, AddressingMode::Indirect_X),
        OpCode::new(0xf1, Mnemonic::SBC, AddressingMode::Indirect_Y),

        // AND
        OpCode::new(0x29, Mnemonic::AND, AddressingMode::Immediate),
        OpCode::new(0x25, Mnemonic::AND, AddressingMode::ZeroPage),
        OpCode::new(0x35, Mnemonic::AND, AddressingMode::ZeroPage_X),
        OpCode::new(0x2d, Mnemonic::AND, AddressingMode::Absolute),
        OpCode::new(0x3d, Mnemonic::AND, AddressingMode::Absolute_X),
        OpCode::new(0x39, Mnemonic::AND, AddressingMode::Absolute_Y),
        OpCode::new(0x21, Mnemonic::AND, AddressingMode::Indirect_X),
        OpCode::new(0x31, Mnemonic::AND, AddressingMode::Indirect_Y),

        // ORA
        OpCode::new(0x09, Mnemonic::ORA, AddressingMode::Immediate),
        OpCode::new(0x05, Mnemonic::ORA, AddressingMode::ZeroPage),
        OpCode::new(0x15, Mnemonic::ORA, AddressingMode::ZeroPage_X),
        OpCode::new(0x0d, Mnemonic::ORA, AddressingMode::Absolute),
        OpCode::new(0x1d, Mnemonic::ORA, AddressingMode::Absolute_X),
        OpCode::new(0x19, Mnemonic::ORA, AddressingMode::Absolute_Y),
        OpCode::new(0x01, Mnemonic::ORA, AddressingMode::Indirect_X),
        OpCode::new(0x11, Mnemonic::ORA, AddressingMode::Indirect_Y),

        // EOR
        OpCode::new(0x49, Mnemonic::EOR, AddressingMode::Immediate),
        OpCode::new(0x45, Mnemonic::EOR, AddressingMode::ZeroPage),
        OpCode::new(0x55, Mnemonic::EOR, AddressingMode::ZeroPage_X),
        OpCode::new(0x4d, Mnemonic::EOR, AddressingMode::Absolute),
        OpCode::new(0x5d, Mnemonic::EOR, AddressingMode::Absolute_X),
        OpCode::new(0x59, Mnemonic::EOR, AddressingMode::Absolute_Y),
        OpCode::new(0x41, Mnemonic::EOR, AddressingMode::Indirect_X),
        OpCode::new(0x51, Mnemonic::EOR, AddressingMode::Indirect_Y),

        // BIT
        OpCode::new(0x24, Mnemonic::BIT, AddressingMode::ZeroPage),
        OpCode::new(0x2c, Mnemonic::BIT, AddressingMode::Absolute),

        // ASL
        OpCode::new(0x0a, Mnemonic::ASL, AddressingMode::NoneAddressing),
        OpCode::new(0x06, Mnemonic::ASL, AddressingMode::ZeroPage),
        OpCode::new(0x16, Mnemonic::ASL, AddressingMode::ZeroPage_X),
        OpCode::new(0x0e, Mnemonic::ASL, AddressingMode::Absolute),
        OpCode::new(0x1e, Mnemonic::ASL, AddressingMode::Absolute_X),

        // LSR
        OpCode::new(0x4a, Mnemonic::LSR, AddressingMode::NoneAddressing),
        OpCode::new(0x46, Mnemonic::LSR, AddressingMode::ZeroPage),
        OpCode::new(0x56, Mnemonic::LSR, AddressingMode::ZeroPage_X),
        OpCode::new(0x4e, Mnemonic::LSR, AddressingMode::Absolute),
        OpCode::new(0x5e, Mnemonic::LSR, AddressingMode::Absolute_X),

        // ROL
        OpCode::new(0x2a, Mnemonic::ROL, AddressingMode::NoneAddressing),
        OpCode::new(0x26, Mnemonic::ROL, AddressingMode::ZeroPage),
        OpCode::new(0x36, Mnemonic::ROL, AddressingMode::ZeroPage_X),
        OpCode::new(0x2e, Mnemonic::ROL, AddressingMode::Absolute),
        OpCode::new(0x3e, Mnemonic::ROL, AddressingMode::Absolute_X),

        // ROR
        OpCode::new(0x6a, Mnemonic::ROR, AddressingMode::NoneAddressing),
        OpCode::new(0x66, Mnemonic::ROR, AddressingMode::ZeroPage),
        OpCode::new(0x76, Mnemonic::ROR, AddressingMode::ZeroPage_X),
        OpCode::new(0x6e, Mnemonic::ROR, AddressingMode::Absolute),
        OpCode::new(0x7e, Mnemonic::ROR, AddressingMode::Absolute_X),

        // CMP
        OpCode::new(0xc9, Mnemonic::CMP, AddressingMode::Immediate),
        OpCode::new(0xc5, Mnemonic::CMP, AddressingMode::ZeroPage),
        OpCode::new(0xd5, Mnemonic::CMP, AddressingMode::ZeroPage_X),
        OpCode::new(0xcd, Mnemonic::CMP, AddressingMode::Absolute),
        OpCode::new(0xdd, Mnemonic::CMP, AddressingMode::Absolute_X),
        OpCode::new(0xd9, Mnemonic::CMP, AddressingMode::Absolute_Y),
        OpCode::new(0xc1, Mnemonic::CMP, AddressingMode::Indirect_X),
        OpCode::new(0xd1, Mnemonic::CMP, AddressingMode::Indirect_Y),

        // CPX
        OpCode::new(0xe0, Mnemonic::CPX, AddressingMode::Immediate),
        OpCode::new(0xe4, Mnemonic::CPX, AddressingMode::ZeroPage),
        OpCode::new(0xec, Mnemonic::CPX, AddressingMode::Absolute),

        // CPY
        OpCode::new(0xc0, Mnemonic::CPY, AddressingMode::Immediate),
        OpCode::new(0xc4, Mnemonic::CPY, AddressingMode::ZeroPage),
        OpCode::new(0xcc, Mnemonic::CPY, AddressingMode::Absolute),

        // DEC
        OpCode::new(0xc6, Mnemonic::DEC, AddressingMode::ZeroPage),
        OpCode::new(0xd6, Mnemonic::DEC, AddressingMode::ZeroPage_X),
        OpCode::new(0xce, Mnemonic::DEC, AddressingMode::Absolute),
        OpCode::new(0xde, Mnemonic::DEC, AddressingMode::Absolute_X),

        // INC
        OpCode::new(0xe6, Mnemonic::INC, AddressingMode::ZeroPage),
        OpCode::new(0xf6, Mnemonic::INC, AddressingMode::ZeroPage_X),
        OpCode::new(0xee, Mnemonic::INC, AddressingMode::Absolute),
        OpCode::new(0xfe, Mnemonic::INC, AddressingMode::Absolute_X),

        // DEX
        OpCode::new(0xca, Mnemonic::DEX, AddressingMode::NoneAddressing),

        // DEY
        OpCode::new(0x88, Mnemonic::DEY, AddressingMode::NoneAddressing),

        // INX
        OpCode::new(0xe8, Mnemonic::INX, AddressingMode::NoneAddressing),

        // INY
        OpCode::new(0xc8, Mnemonic::INY, AddressingMode::NoneAddressing),

        // PHA
        OpCode::new(0x48, Mnemonic::PHA, AddressingMode::NoneAddressing),

        // PHP
        OpCode::new(0x08, Mnemonic::PHP, AddressingMode::NoneAddressing),

        // PLA
        OpCode::new(0x68, Mnemonic::PLA, AddressingMode::NoneAddressing),
    ];

    pub static ref OPCODES_MAP: HashMap<u8, &'static OpCode> = {
        let mut map = HashMap::new();
        for cpuop in &*CPU_OPS_CODES {
            map.insert(cpuop.code, cpuop);
        }
        map
    };
}