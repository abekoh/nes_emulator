use std::collections::HashMap;

use crate::cpu::AddressingMode;

pub struct OpCode {
    pub code: u8,
    pub len: u8,
    pub mode: AddressingMode,
}

impl OpCode {
    fn new(code: u8, len: u8, mode: AddressingMode) -> Self {
        OpCode {
            code,
            len,
            mode,
        }
    }
}

lazy_static! {
    pub static ref CPU_OPS_CODES: Vec<OpCode> = vec![
        // BRK
        OpCode::new(0x00, 1, AddressingMode::NoneAddressing),

        // LDA
        OpCode::new(0xa9, 2, AddressingMode::Immediate),
        OpCode::new(0xa5, 2, AddressingMode::ZeroPage),
        OpCode::new(0xb5, 2, AddressingMode::ZeroPage_X),
    ];

    pub static ref OPCODES_MAP: HashMap<u8, &'static OpCode> = {
        let mut map = HashMap::new();
        for cpuop in &*CPU_OPS_CODES {
            map.insert(cpuop.code, cpuop);
        }
        map
    };
}