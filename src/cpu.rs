use std::collections::HashMap;

use crate::opcodes;
use crate::opcodes::Mnemonic;

pub struct CPU {
    pub a: u8,
    pub x: u8,
    pub y: u8,
    // pub stack_pointer: u8,
    pub status: u8,
    pub pc: u16,
    mem: [u8; 0xFFFF],
}

#[derive(Debug)]
#[allow(non_camel_case_types)]
pub enum AddressingMode {
    Immediate,
    ZeroPage,
    ZeroPage_X,
    ZeroPage_Y,
    Absolute,
    Absolute_X,
    Absolute_Y,
    Indirect_X,
    Indirect_Y,
    NoneAddressing,
}

impl CPU {
    pub fn new() -> Self {
        CPU {
            a: 0,
            x: 0,
            y: 0,
            status: 0,
            pc: 0,
            mem: [0; 0xFFFF],
        }
    }

    fn mem_read(&self, addr: u16) -> u8 {
        self.mem[addr as usize]
    }

    fn mem_write(&mut self, addr: u16, data: u8) {
        self.mem[addr as usize] = data;
    }

    fn mem_read_u16(&self, pos: u16) -> u16 {
        let lo = self.mem_read(pos) as u16;
        let hi = self.mem_read(pos + 1) as u16;
        (hi << 8) | (lo as u16)
    }

    fn mem_write_u16(&mut self, pos: u16, data: u16) {
        let hi = (data >> 8) as u8;
        let lo = (data & 0xff) as u8;
        self.mem_write(pos, lo);
        self.mem_write(pos + 1, hi);
    }

    pub fn reset(&mut self) {
        self.a = 0;
        self.x = 0;
        self.status = 0;

        self.pc = self.mem_read_u16(0xFFFC);
    }

    pub fn load_and_run(&mut self, program: Vec<u8>) {
        self.load(program);
        self.reset();
        self.run();
    }

    pub fn load(&mut self, program: Vec<u8>) {
        self.mem[0x8000..(0x8000 + program.len())].copy_from_slice(&program[..]);
        self.mem_write_u16(0xFFFC, 0x8000);
    }

    pub fn run(&mut self) {
        let ref opcodes: HashMap<u8, &'static opcodes::OpCode> = *opcodes::OPCODES_MAP;
        loop {
            let code = self.mem_read(self.pc);
            self.pc += 1;

            let opcode = opcodes.get(&code).expect(&format!("OpCode {:x} is not recognized", code));

            match &(opcode.mnemonic) {
                Mnemonic::LDA => {
                    self.lda(&opcode.mode);
                }
                Mnemonic::LDX => {
                    self.ldx(&opcode.mode);
                }
                Mnemonic::LDY => {
                    self.ldy(&opcode.mode);
                }
                Mnemonic::BRK => return,
            }
            self.pc += opcode.pc_offset() as u16;
        }
    }

    fn get_operand_address(&self, mode: &AddressingMode) -> u16 {
        match mode {
            AddressingMode::Immediate => self.pc,
            AddressingMode::ZeroPage => self.mem_read(self.pc) as u16,
            AddressingMode::Absolute => self.mem_read_u16(self.pc),
            AddressingMode::ZeroPage_X => {
                let pos = self.mem_read(self.pc);
                let addr = pos.wrapping_add(self.x) as u16;
                addr
            }
            AddressingMode::ZeroPage_Y => {
                let pos = self.mem_read(self.pc);
                pos.wrapping_add(self.y) as u16
            }
            AddressingMode::Absolute_X => {
                let pos = self.mem_read_u16(self.pc);
                pos.wrapping_add(self.x as u16)
            }
            AddressingMode::Absolute_Y => {
                let pos = self.mem_read_u16(self.pc);
                pos.wrapping_add(self.y as u16)
            }
            AddressingMode::Indirect_X => {
                let base = self.mem_read(self.pc);

                let ptr: u8 = (base as u8).wrapping_add(self.x);
                let lo = self.mem_read(ptr as u16);
                let hi = self.mem_read(ptr.wrapping_add(1) as u16);
                (hi as u16) << 8 | (lo as u16)
            }
            AddressingMode::Indirect_Y => {
                let base = self.mem_read(self.pc);

                let lo = self.mem_read(base as u16);
                let hi = self.mem_read((base as u16).wrapping_add(1) as u16);
                let deref_base = (hi as u16) << 8 | (lo as u16);
                let deref = deref_base.wrapping_add(self.y as u16);
                deref
            }
            AddressingMode::NoneAddressing => {
                panic!("mode {:?} is not supported", mode);
            }
        }
    }

    fn lda(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        self.a = value;
        self.update_zero_flags(self.a);
        self.update_negative_flags(self.a);
    }

    fn ldx(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        self.x = value;
        self.update_zero_flags(self.a);
        self.update_negative_flags(self.a);
    }

    fn ldy(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        self.y = value;
        self.update_zero_flags(self.a);
        self.update_negative_flags(self.a);
    }

    fn update_zero_flags(&mut self, result: u8) {
        if result == 0 {
            self.status = self.status | 0b0000_0010;
        } else {
            self.status = self.status & 0b1111_1101;
        }
    }

    fn update_negative_flags(&mut self, result: u8) {
        if result & 0b1000_0000 != 0 {
            self.status = self.status | 0b1000_0000;
        } else {
            self.status = self.status & 0b0111_1111;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[cfg(test)]
    mod status {
        use super::*;

        #[test]
        fn zero_on() {
            let mut cpu = CPU::new();
            // LDA #$00
            cpu.load_and_run(vec![0xa9, 0x00, 0x00]);
            assert_eq!(cpu.status & 0b0000_0010, 0b10);
        }
    }

    #[cfg(test)]
    mod lda {
        use super::*;

        #[test]
        fn immediate() {
            let mut cpu = CPU::new();
            cpu.load_and_run(vec![0xa9, 0x11, 0x00]);
            assert_eq!(cpu.a, 0x11);
        }

        #[test]
        fn zeropage() {
            let mut cpu = CPU::new();
            cpu.mem_write(0x10, 0x55);
            cpu.load_and_run(vec![0xa5, 0x10, 0x00]);
            assert_eq!(cpu.a, 0x55);
        }

        #[test]
        fn zeropage_x() {
            let mut cpu = CPU::new();
            cpu.mem_write(0x11, 0x55);
            cpu.load(vec![0xb5, 0x10, 0x00]);
            cpu.reset();
            cpu.x = 0x01;
            cpu.run();
            assert_eq!(cpu.a, 0x55);
        }

        #[test]
        fn absolute() {
            let mut cpu = CPU::new();
            cpu.mem_write(0x1122, 0x55);
            cpu.load_and_run(vec![0xad, 0x22, 0x11, 0x00]);
            assert_eq!(cpu.a, 0x55);
        }

        #[test]
        fn absolute_x() {
            let mut cpu = CPU::new();
            cpu.mem_write(0x1133, 0x55);
            cpu.load(vec![0xbd, 0x22, 0x11, 0x00]);
            cpu.reset();
            cpu.x = 0x11;
            cpu.run();
            assert_eq!(cpu.a, 0x55);
        }

        #[test]
        fn absolute_y() {
            let mut cpu = CPU::new();
            cpu.mem_write(0x1133, 0x55);
            cpu.load(vec![0xb9, 0x22, 0x11, 0x00]);
            cpu.reset();
            cpu.y = 0x11;
            cpu.run();
            assert_eq!(cpu.a, 0x55);
        }

        #[test]
        fn indirect_x() {
            let mut cpu = CPU::new();
            cpu.mem_write(0x0705, 0x0a);
            cpu.mem_write(0x01, 0x05);
            cpu.mem_write(0x02, 0x07);
            cpu.load(vec![0xa1, 0x00, 0x00]);
            cpu.reset();
            cpu.x = 0x01;
            cpu.run();
            assert_eq!(cpu.a, 0x0a);
        }

        #[test]
        fn indirect_y() {
            let mut cpu = CPU::new();
            cpu.mem_write(0x0704, 0x0a);
            cpu.mem_write(0x01, 0x03);
            cpu.mem_write(0x02, 0x07);
            cpu.load(vec![0xb1, 0x01, 0x00]);
            cpu.reset();
            cpu.y = 0x01;
            cpu.run();
            assert_eq!(cpu.a, 0x0a);
        }
    }

    #[cfg(test)]
    mod ldx {
        use super::*;

        #[test]
        fn immediate() {
            let mut cpu = CPU::new();
            cpu.load_and_run(vec![0xa2, 0x11, 0x00]);
            assert_eq!(cpu.x, 0x11);
        }
    }

    #[cfg(test)]
    mod ldy {
        use super::*;

        #[test]
        fn immediate() {
            let mut cpu = CPU::new();
            cpu.load_and_run(vec![0xa0, 0x11, 0x00]);
            assert_eq!(cpu.y, 0x11);
        }
    }
}
