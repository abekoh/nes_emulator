use std::collections::HashMap;

use crate::opcodes;
use crate::opcodes::Mnemonic;

pub struct CPU {
    pub a: u8,
    pub x: u8,
    pub y: u8,
    pub sp: u8,
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

#[allow(non_camel_case_types, dead_code)]
enum Flag {
    Carry,
    Zero,
    IRQ_Limited,
    Decimal,
    Break,
    Reserved,
    OverFlow,
    Negative,
}

enum Register {
    A,
    X,
    Y,
    S,
}

impl Flag {
    fn place(&self) -> u8 {
        match self {
            Flag::Carry => 0b0000_0001,
            Flag::Zero => 0b0000_0010,
            Flag::IRQ_Limited => 0b0000_0100,
            Flag::Decimal => 0b0000_1000,
            Flag::Break => 0b0001_0000,
            Flag::Reserved => 0b0010_0000,
            Flag::OverFlow => 0b0100_0000,
            Flag::Negative => 0b1000_0000,
        }
    }
}

#[derive(PartialEq)]
enum IntType {
    Positive,
    Negative,
}

fn int_type(val: u8) -> IntType {
    if val & 0b1000_0000 != 0 {
        IntType::Negative
    } else {
        IntType::Positive
    }
}

impl CPU {
    pub fn new() -> Self {
        CPU {
            a: 0,
            x: 0,
            y: 0,
            sp: 0,
            status: 0,
            pc: 0,
            mem: [0; 0xFFFF],
        }
    }

    pub fn mem_read(&self, addr: u16) -> u8 {
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

    pub fn load_reset(&mut self, program: Vec<u8>) {
        self.load(program);
        self.reset();
    }

    pub fn load_reset_run(&mut self, program: Vec<u8>) {
        self.load(program);
        self.reset();
        self.run();
    }

    fn load(&mut self, program: Vec<u8>) {
        self.mem[0x8000..(0x8000 + program.len())].copy_from_slice(&program[..]);
        self.mem_write_u16(0xFFFC, 0x8000);
    }

    fn reset(&mut self) {
        self.a = 0;
        self.x = 0;
        self.status = 0;
        self.pc = self.mem_read_u16(0xFFFC);
    }

    pub fn run(&mut self) {
        let ref opcodes: HashMap<u8, &'static opcodes::OpCode> = *opcodes::OPCODES_MAP;
        loop {
            let code = self.mem_read(self.pc);
            self.pc += 1;

            let opcode = opcodes.get(&code).expect(&format!("OpCode {:x} is not recognized", code));

            match &(opcode.mnemonic) {
                Mnemonic::LDA => self.ld(&Register::A, &opcode.mode),
                Mnemonic::LDX => self.ld(&Register::X, &opcode.mode),
                Mnemonic::LDY => self.ld(&Register::Y, &opcode.mode),
                Mnemonic::STA => self.st(&Register::A, &opcode.mode),
                Mnemonic::STX => self.st(&Register::X, &opcode.mode),
                Mnemonic::STY => self.st(&Register::Y, &opcode.mode),
                Mnemonic::TAX => self.t(&Register::A, &Register::X),
                Mnemonic::TAY => self.t(&Register::A, &Register::Y),
                Mnemonic::TSX => self.t(&Register::S, &Register::X),
                Mnemonic::TXA => self.t(&Register::X, &Register::A),
                Mnemonic::TXS => self.t(&Register::X, &Register::S),
                Mnemonic::TYA => self.t(&Register::Y, &Register::A),
                Mnemonic::ADC => self.adc(&opcode.mode),
                Mnemonic::SBC => self.sbc(&opcode.mode),
                Mnemonic::AND => self.and(&opcode.mode),
                Mnemonic::ASL => self.asl(&opcode.mode),
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

    fn get_register(&self, reg: &Register) -> u8 {
        match reg {
            Register::A => self.a,
            Register::X => self.x,
            Register::Y => self.y,
            Register::S => self.sp,
        }
    }

    fn set_register(&mut self, reg: &Register, data: u8) {
        match reg {
            Register::A => self.a = data,
            Register::X => self.x = data,
            Register::Y => self.y = data,
            Register::S => self.sp = data,
        };
    }

    fn set_register_with_update_flags(&mut self, reg: &Register, data: u8) {
        self.set_register(reg, data);
        self.update_zero_flag(data);
        self.update_negative_flag(data);
    }

    fn ld(&mut self, reg: &Register, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let val = self.mem_read(addr);
        self.set_register_with_update_flags(reg, val);
    }

    fn st(&mut self, reg: &Register, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        self.mem_write(addr, self.get_register(reg));
    }

    fn t(&mut self, from: &Register, to: &Register) {
        let val = self.get_register(from);
        self.set_register_with_update_flags(to, val);
    }

    fn add_to_a(&mut self, param: u8) {
        let carry_val: u8 = if self.get_flag(&Flag::Carry) { 1 } else { 0 };
        let (res, over1) = self.a.overflowing_add(param);
        let (res, over2) = res.overflowing_add(carry_val);
        self.set_flag(&Flag::Carry, over1 || over2);
        self.set_flag(&Flag::OverFlow, (res ^ param) & (res ^ self.a) & 0b1000_0000 != 0);
        self.set_register_with_update_flags(&Register::A, res);
    }

    fn adc(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let mem_val = self.mem_read(addr);
        self.add_to_a(mem_val);
    }

    fn sbc(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let mem_val = self.mem_read(addr);
        self.add_to_a((mem_val as i8).wrapping_neg().wrapping_sub(1) as u8);
    }

    fn and(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let mem_val = self.mem_read(addr);
        let val = self.a & mem_val;
        self.set_register_with_update_flags(&Register::A, val);
    }

    fn asl(&mut self, mode: &AddressingMode) {
        // TODO: addressing
        match mode {
            AddressingMode::NoneAddressing => {
                let (res, over) = self.a.overflowing_shl(1);
                self.set_flag(&Flag::Carry, over);
                self.set_register_with_update_flags(&Register::A, res);
            }
            _ => todo!()
        };
    }

    fn update_zero_flag(&mut self, result: u8) {
        self.set_flag(&Flag::Zero, result == 0);
    }

    fn update_negative_flag(&mut self, result: u8) {
        self.set_flag(&Flag::Negative, int_type(result) == IntType::Negative);
    }

    fn get_flag(&self, flag: &Flag) -> bool {
        (self.status & flag.place()) > 0
    }

    fn set_flag(&mut self, flag: &Flag, val: bool) {
        if val {
            self.status = self.status | flag.place();
        } else {
            self.status = self.status & !flag.place();
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
            cpu.load_reset_run(vec![0xa9, 0x00, 0x00]);
            assert_eq!(cpu.status & 0b0000_0010, 0b10);
        }

        #[test]
        fn zero_off() {
            let mut cpu = CPU::new();
            // LDA #$01
            cpu.load_reset_run(vec![0xa9, 0x01, 0x00]);
            assert_eq!(cpu.status & 0b0000_0010, 0b00);
        }

        #[test]
        fn negative_on() {
            let mut cpu = CPU::new();
            // LDA #$ff
            cpu.load_reset_run(vec![0xa9, 0xff, 0x00]);
            assert_eq!(cpu.status & 0b1000_0000, 0b1000_0000);
        }

        #[test]
        fn negative_off() {
            let mut cpu = CPU::new();
            // LDA #$01
            cpu.load_reset_run(vec![0xa9, 0x01, 0x00]);
            assert_eq!(cpu.status & 0b1000_0000, 0b0000_0000);
        }

        #[test]
        fn carry_on() {
            let mut cpu = CPU::new();
            // ADC #$ff
            cpu.load_reset(vec![0x69, 0xff, 0x00]);
            cpu.a = 0x01;
            cpu.run();
            assert_eq!(cpu.status & 0b0000_0001, 0b0000_0001);
        }

        #[test]
        fn carry_off() {
            let mut cpu = CPU::new();
            // ADC #$01
            cpu.load_reset(vec![0x69, 0x01, 0x00]);
            cpu.a = 0x01;
            cpu.run();
            assert_eq!(cpu.status & 0b0000_0001, 0b0000_0000);
        }

        #[test]
        fn overflow_on() {
            let mut cpu = CPU::new();
            cpu.load_reset(vec![0x69, 0b0100_0000, 0x00]);
            cpu.a = 0b0100_0000;
            cpu.run();
            assert_eq!(cpu.status & 0b0100_0000, 0b0100_0000);
        }

        #[test]
        fn overflow_off() {
            let mut cpu = CPU::new();
            // ADC #$ff
            cpu.load_reset(vec![0x69, 0xff, 0x00]);
            cpu.a = 0x01;
            cpu.run();
            assert_eq!(cpu.status & 0b0100_0000, 0b0000_0000);
        }
    }

    #[cfg(test)]
    mod lda {
        use super::*;

        #[test]
        fn immediate() {
            let mut cpu = CPU::new();
            cpu.load_reset_run(vec![0xa9, 0x11, 0x00]);
            assert_eq!(cpu.a, 0x11);
        }

        #[test]
        fn zeropage() {
            let mut cpu = CPU::new();
            cpu.mem_write(0x10, 0x55);
            cpu.load_reset_run(vec![0xa5, 0x10, 0x00]);
            assert_eq!(cpu.a, 0x55);
        }

        #[test]
        fn zeropage_x() {
            let mut cpu = CPU::new();
            cpu.mem_write(0x11, 0x55);
            cpu.load_reset(vec![0xb5, 0x10, 0x00]);
            cpu.reset();
            cpu.x = 0x01;
            cpu.run();
            assert_eq!(cpu.a, 0x55);
        }

        #[test]
        fn absolute() {
            let mut cpu = CPU::new();
            cpu.mem_write(0x1122, 0x55);
            cpu.load_reset_run(vec![0xad, 0x22, 0x11, 0x00]);
            assert_eq!(cpu.a, 0x55);
        }

        #[test]
        fn absolute_x() {
            let mut cpu = CPU::new();
            cpu.mem_write(0x1133, 0x55);
            cpu.load_reset(vec![0xbd, 0x22, 0x11, 0x00]);
            cpu.reset();
            cpu.x = 0x11;
            cpu.run();
            assert_eq!(cpu.a, 0x55);
        }

        #[test]
        fn absolute_y() {
            let mut cpu = CPU::new();
            cpu.mem_write(0x1133, 0x55);
            cpu.load_reset(vec![0xb9, 0x22, 0x11, 0x00]);
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
            cpu.load_reset(vec![0xa1, 0x00, 0x00]);
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
            cpu.load_reset(vec![0xb1, 0x01, 0x00]);
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
            cpu.load_reset_run(vec![0xa2, 0x11, 0x00]);
            assert_eq!(cpu.x, 0x11);
        }

        #[test]
        fn zeropage() {
            let mut cpu = CPU::new();
            cpu.mem_write(0x10, 0x55);
            cpu.load_reset_run(vec![0xa6, 0x10, 0x00]);
            assert_eq!(cpu.x, 0x55);
        }

        #[test]
        fn zeropage_y() {
            let mut cpu = CPU::new();
            cpu.mem_write(0x11, 0x55);
            cpu.load_reset(vec![0xb6, 0x10, 0x00]);
            cpu.y = 0x01;
            cpu.run();
            assert_eq!(cpu.x, 0x55);
        }

        #[test]
        fn absolute() {
            let mut cpu = CPU::new();
            cpu.mem_write(0x1122, 0x55);
            cpu.load_reset_run(vec![0xae, 0x22, 0x11, 0x00]);
            assert_eq!(cpu.x, 0x55);
        }

        #[test]
        fn absolute_y() {
            let mut cpu = CPU::new();
            cpu.mem_write(0x1133, 0x55);
            cpu.load_reset(vec![0xbe, 0x22, 0x11, 0x00]);
            cpu.y = 0x11;
            cpu.run();
            assert_eq!(cpu.x, 0x55);
        }
    }

    #[cfg(test)]
    mod ldy {
        use super::*;

        #[test]
        fn immediate() {
            let mut cpu = CPU::new();
            cpu.load_reset_run(vec![0xa0, 0x11, 0x00]);
            assert_eq!(cpu.y, 0x11);
        }

        #[test]
        fn zeropage() {
            let mut cpu = CPU::new();
            cpu.mem_write(0x10, 0x55);
            cpu.load_reset_run(vec![0xa4, 0x10, 0x00]);
            assert_eq!(cpu.y, 0x55);
        }

        #[test]
        fn zeropage_x() {
            let mut cpu = CPU::new();
            cpu.mem_write(0x11, 0x55);
            cpu.load_reset(vec![0xb4, 0x10, 0x00]);
            cpu.x = 0x01;
            cpu.run();
            assert_eq!(cpu.y, 0x55);
        }

        #[test]
        fn absolute() {
            let mut cpu = CPU::new();
            cpu.mem_write(0x1122, 0x55);
            cpu.load_reset_run(vec![0xac, 0x22, 0x11, 0x00]);
            assert_eq!(cpu.y, 0x55);
        }

        #[test]
        fn absolute_x() {
            let mut cpu = CPU::new();
            cpu.mem_write(0x1133, 0x55);
            cpu.load_reset(vec![0xbc, 0x22, 0x11, 0x00]);
            cpu.x = 0x11;
            cpu.run();
            assert_eq!(cpu.y, 0x55);
        }
    }

    #[cfg(test)]
    mod sta {
        use super::*;

        #[test]
        fn zeropage() {
            let mut cpu = CPU::new();
            cpu.load_reset(vec![0x85, 0x01, 0x00]);
            cpu.a = 0x55;
            cpu.run();
            assert_eq!(cpu.mem_read(0x01), 0x55);
        }

        #[test]
        fn zeropage_x() {
            let mut cpu = CPU::new();
            cpu.load_reset(vec![0x95, 0x01, 0x00]);
            cpu.a = 0x55;
            cpu.x = 0x01;
            cpu.run();
            assert_eq!(cpu.mem_read(0x02), 0x55);
        }

        #[test]
        fn absolute() {
            let mut cpu = CPU::new();
            cpu.load_reset(vec![0x8d, 0x22, 0x11, 0x00]);
            cpu.a = 0x55;
            cpu.run();
            assert_eq!(cpu.mem_read(0x1122), 0x55);
        }

        #[test]
        fn absolute_x() {
            let mut cpu = CPU::new();
            cpu.load_reset(vec![0x9d, 0x22, 0x11, 0x00]);
            cpu.a = 0x55;
            cpu.x = 0x33;
            cpu.run();
            assert_eq!(cpu.mem_read(0x1155), 0x55);
        }

        #[test]
        fn absolute_y() {
            let mut cpu = CPU::new();
            cpu.load_reset(vec![0x99, 0x22, 0x11, 0x00]);
            cpu.a = 0x55;
            cpu.y = 0x33;
            cpu.run();
            assert_eq!(cpu.mem_read(0x1155), 0x55);
        }

        #[test]
        fn indirect_x() {
            let mut cpu = CPU::new();
            cpu.mem_write(0x01, 0x05);
            cpu.mem_write(0x02, 0x07);
            cpu.load_reset(vec![0x81, 0x00, 0x00]);
            cpu.a = 0x55;
            cpu.x = 0x01;
            cpu.run();
            assert_eq!(cpu.mem_read(0x0705), 0x55);
        }

        #[test]
        fn indirect_y() {
            let mut cpu = CPU::new();
            cpu.mem_write(0x01, 0x03);
            cpu.mem_write(0x02, 0x07);
            cpu.load_reset(vec![0x91, 0x01, 0x00]);
            cpu.a = 0x55;
            cpu.y = 0x01;
            cpu.run();
            assert_eq!(cpu.mem_read(0x0704), 0x55);
        }
    }

    #[cfg(test)]
    mod stx {
        use super::*;

        #[test]
        fn zeropage() {
            let mut cpu = CPU::new();
            cpu.load_reset(vec![0x86, 0x01, 0x00]);
            cpu.x = 0x55;
            cpu.run();
            assert_eq!(cpu.mem_read(0x01), 0x55);
        }

        #[test]
        fn zeropage_y() {
            let mut cpu = CPU::new();
            cpu.load_reset(vec![0x96, 0x01, 0x00]);
            cpu.x = 0x55;
            cpu.y = 0x01;
            cpu.run();
            assert_eq!(cpu.mem_read(0x02), 0x55);
        }

        #[test]
        fn absolute() {
            let mut cpu = CPU::new();
            cpu.load_reset(vec![0x8e, 0x22, 0x11, 0x00]);
            cpu.x = 0x55;
            cpu.run();
            assert_eq!(cpu.mem_read(0x1122), 0x55);
        }
    }

    #[cfg(test)]
    mod sty {
        use super::*;

        #[test]
        fn zeropage() {
            let mut cpu = CPU::new();
            cpu.load_reset(vec![0x84, 0x01, 0x00]);
            cpu.y = 0x55;
            cpu.run();
            assert_eq!(cpu.mem_read(0x01), 0x55);
        }

        #[test]
        fn zeropage_y() {
            let mut cpu = CPU::new();
            cpu.load_reset(vec![0x94, 0x01, 0x00]);
            cpu.x = 0x01;
            cpu.y = 0x55;
            cpu.run();
            assert_eq!(cpu.mem_read(0x02), 0x55);
        }

        #[test]
        fn absolute() {
            let mut cpu = CPU::new();
            cpu.load_reset(vec![0x8c, 0x22, 0x11, 0x00]);
            cpu.y = 0x55;
            cpu.run();
            assert_eq!(cpu.mem_read(0x1122), 0x55);
        }
    }

    #[test]
    fn tax() {
        let mut cpu = CPU::new();
        cpu.load_reset(vec![0xaa, 0x00]);
        cpu.a = 0x55;
        cpu.run();
        assert_eq!(cpu.x, 0x55);
    }

    #[test]
    fn tay() {
        let mut cpu = CPU::new();
        cpu.load_reset(vec![0xa8, 0x00]);
        cpu.a = 0x55;
        cpu.run();
        assert_eq!(cpu.y, 0x55);
    }

    #[test]
    fn tsx() {
        let mut cpu = CPU::new();
        cpu.load_reset(vec![0xba, 0x00]);
        cpu.sp = 0x55;
        cpu.run();
        assert_eq!(cpu.x, 0x55);
    }

    #[test]
    fn txa() {
        let mut cpu = CPU::new();
        cpu.load_reset(vec![0x8a, 0x00]);
        cpu.x = 0x55;
        cpu.run();
        assert_eq!(cpu.a, 0x55);
    }

    #[test]
    fn txs() {
        let mut cpu = CPU::new();
        cpu.load_reset(vec![0x9a, 0x00]);
        cpu.x = 0x55;
        cpu.run();
        assert_eq!(cpu.sp, 0x55);
    }

    #[test]
    fn tya() {
        let mut cpu = CPU::new();
        cpu.load_reset(vec![0x98, 0x00]);
        cpu.y = 0x55;
        cpu.run();
        assert_eq!(cpu.a, 0x55);
    }

    #[cfg(test)]
    mod adc {
        use super::*;

        #[test]
        fn immediate_no_carry_flag() {
            let mut cpu = CPU::new();
            cpu.load_reset(vec![0x69, 0x11, 0x00]);
            cpu.a = 0x22;
            cpu.run();
            assert_eq!(cpu.a, 0x33);
        }

        #[test]
        fn immediate_with_carry_flag() {
            let mut cpu = CPU::new();
            cpu.load_reset(vec![0x69, 0x11, 0x00]);
            cpu.a = 0x22;
            cpu.set_flag(&Flag::Carry, true);
            cpu.run();
            assert_eq!(cpu.a, 0x34);
        }

        #[test]
        fn zeropage() {
            let mut cpu = CPU::new();
            cpu.mem_write(0x10, 0x11);
            cpu.load_reset(vec![0x65, 0x10, 0x00]);
            cpu.a = 0x22;
            cpu.run();
            assert_eq!(cpu.a, 0x33);
        }

        #[test]
        fn zeropage_x() {
            let mut cpu = CPU::new();
            cpu.mem_write(0x11, 0x33);
            cpu.load_reset(vec![0x75, 0x10, 0x00]);
            cpu.reset();
            cpu.a = 0x22;
            cpu.x = 0x01;
            cpu.run();
            assert_eq!(cpu.a, 0x55);
        }

        #[test]
        fn absolute() {
            let mut cpu = CPU::new();
            cpu.mem_write(0x1122, 0x11);
            cpu.load_reset(vec![0x6d, 0x22, 0x11, 0x00]);
            cpu.a = 0x22;
            cpu.run();
            assert_eq!(cpu.a, 0x33);
        }

        #[test]
        fn absolute_x() {
            let mut cpu = CPU::new();
            cpu.mem_write(0x1133, 0x44);
            cpu.load_reset(vec![0x7d, 0x22, 0x11, 0x00]);
            cpu.reset();
            cpu.a = 0x22;
            cpu.x = 0x11;
            cpu.run();
            assert_eq!(cpu.a, 0x66);
        }

        #[test]
        fn absolute_y() {
            let mut cpu = CPU::new();
            cpu.mem_write(0x1133, 0x44);
            cpu.load_reset(vec![0x79, 0x22, 0x11, 0x00]);
            cpu.reset();
            cpu.a = 0x22;
            cpu.y = 0x11;
            cpu.run();
            assert_eq!(cpu.a, 0x66);
        }

        #[test]
        fn indirect_x() {
            let mut cpu = CPU::new();
            cpu.mem_write(0x0705, 0x44);
            cpu.mem_write(0x01, 0x05);
            cpu.mem_write(0x02, 0x07);
            cpu.load_reset(vec![0x61, 0x00, 0x00]);
            cpu.reset();
            cpu.a = 0x22;
            cpu.x = 0x01;
            cpu.run();
            assert_eq!(cpu.a, 0x66);
        }

        #[test]
        fn indirect_y() {
            let mut cpu = CPU::new();
            cpu.mem_write(0x0704, 0x44);
            cpu.mem_write(0x01, 0x03);
            cpu.mem_write(0x02, 0x07);
            cpu.load_reset(vec![0x71, 0x01, 0x00]);
            cpu.reset();
            cpu.a = 0x22;
            cpu.y = 0x01;
            cpu.run();
            assert_eq!(cpu.a, 0x66);
        }
    }

    #[cfg(test)]
    mod sbc {
        use super::*;

        #[test]
        fn immediate_no_carry_flag() {
            let mut cpu = CPU::new();
            cpu.load_reset(vec![0xe9, 0x11, 0x00]);
            cpu.a = 0x33;
            cpu.run();
            // 0x33 - 0x11 - 0x01 (reversed carry flag)
            assert_eq!(cpu.a, 0x21);
        }

        #[test]
        fn immediate_with_carry_flag() {
            let mut cpu = CPU::new();
            cpu.load_reset(vec![0xe9, 0x11, 0x00]);
            cpu.a = 0x33;
            cpu.set_flag(&Flag::Carry, true);
            cpu.run();
            // 0x33 - 0x11
            assert_eq!(cpu.a, 0x22);
        }

        #[test]
        fn zeropage() {
            let mut cpu = CPU::new();
            cpu.mem_write(0x10, 0x11);
            cpu.load_reset(vec![0xe5, 0x10, 0x00]);
            cpu.a = 0x33;
            cpu.run();
            assert_eq!(cpu.a, 0x21);
        }

        #[test]
        fn zeropage_x() {
            let mut cpu = CPU::new();
            cpu.mem_write(0x11, 0x11);
            cpu.load_reset(vec![0xf5, 0x10, 0x00]);
            cpu.reset();
            cpu.a = 0x33;
            cpu.x = 0x01;
            cpu.run();
            assert_eq!(cpu.a, 0x21);
        }

        #[test]
        fn absolute() {
            let mut cpu = CPU::new();
            cpu.mem_write(0x1122, 0x11);
            cpu.load_reset(vec![0xed, 0x22, 0x11, 0x00]);
            cpu.a = 0x33;
            cpu.run();
            assert_eq!(cpu.a, 0x21);
        }

        #[test]
        fn absolute_x() {
            let mut cpu = CPU::new();
            cpu.mem_write(0x1133, 0x11);
            cpu.load_reset(vec![0xfd, 0x22, 0x11, 0x00]);
            cpu.reset();
            cpu.a = 0x33;
            cpu.x = 0x11;
            cpu.run();
            assert_eq!(cpu.a, 0x21);
        }

        #[test]
        fn absolute_y() {
            let mut cpu = CPU::new();
            cpu.mem_write(0x1133, 0x11);
            cpu.load_reset(vec![0xf9, 0x22, 0x11, 0x00]);
            cpu.reset();
            cpu.a = 0x33;
            cpu.y = 0x11;
            cpu.run();
            assert_eq!(cpu.a, 0x21);
        }

        #[test]
        fn indirect_x() {
            let mut cpu = CPU::new();
            cpu.mem_write(0x0705, 0x11);
            cpu.mem_write(0x01, 0x05);
            cpu.mem_write(0x02, 0x07);
            cpu.load_reset(vec![0xe1, 0x00, 0x00]);
            cpu.reset();
            cpu.a = 0x33;
            cpu.x = 0x01;
            cpu.run();
            assert_eq!(cpu.a, 0x21);
        }

        #[test]
        fn indirect_y() {
            let mut cpu = CPU::new();
            cpu.mem_write(0x0704, 0x11);
            cpu.mem_write(0x01, 0x03);
            cpu.mem_write(0x02, 0x07);
            cpu.load_reset(vec![0xf1, 0x01, 0x00]);
            cpu.reset();
            cpu.a = 0x33;
            cpu.y = 0x01;
            cpu.run();
            assert_eq!(cpu.a, 0x21);
        }
    }

    #[cfg(test)]
    mod and {
        use super::*;

        #[test]
        fn immediate() {
            let mut cpu = CPU::new();
            cpu.load_reset(vec![0x29, 0b0101, 0x00]);
            cpu.a = 0b1100;
            cpu.run();
            assert_eq!(cpu.a, 0b0100);
        }

        #[test]
        fn zeropage() {
            let mut cpu = CPU::new();
            cpu.mem_write(0x10, 0b0101);
            cpu.load_reset(vec![0x25, 0x10, 0x00]);
            cpu.a = 0b1100;
            cpu.run();
            assert_eq!(cpu.a, 0b0100);
        }

        #[test]
        fn zeropage_x() {
            let mut cpu = CPU::new();
            cpu.mem_write(0x11, 0b0101);
            cpu.load_reset(vec![0x35, 0x10, 0x00]);
            cpu.reset();
            cpu.a = 0b1100;
            cpu.x = 0x01;
            cpu.run();
            assert_eq!(cpu.a, 0b0100);
        }

        #[test]
        fn absolute() {
            let mut cpu = CPU::new();
            cpu.mem_write(0x1122, 0b0101);
            cpu.load_reset(vec![0x2d, 0x22, 0x11, 0x00]);
            cpu.a = 0b1100;
            cpu.run();
            assert_eq!(cpu.a, 0b0100);
        }

        #[test]
        fn absolute_x() {
            let mut cpu = CPU::new();
            cpu.mem_write(0x1133, 0b0101);
            cpu.load_reset(vec![0x3d, 0x22, 0x11, 0x00]);
            cpu.reset();
            cpu.a = 0b1100;
            cpu.x = 0x11;
            cpu.run();
            assert_eq!(cpu.a, 0b0100);
        }

        #[test]
        fn absolute_y() {
            let mut cpu = CPU::new();
            cpu.mem_write(0x1133, 0b0101);
            cpu.load_reset(vec![0x39, 0x22, 0x11, 0x00]);
            cpu.reset();
            cpu.a = 0b1100;
            cpu.y = 0x11;
            cpu.run();
            assert_eq!(cpu.a, 0b0100);
        }

        #[test]
        fn indirect_x() {
            let mut cpu = CPU::new();
            cpu.mem_write(0x0705, 0b0101);
            cpu.mem_write(0x01, 0x05);
            cpu.mem_write(0x02, 0x07);
            cpu.load_reset(vec![0x21, 0x00, 0x00]);
            cpu.reset();
            cpu.a = 0b1100;
            cpu.x = 0x01;
            cpu.run();
            assert_eq!(cpu.a, 0b0100);
        }

        #[test]
        fn indirect_y() {
            let mut cpu = CPU::new();
            cpu.mem_write(0x0704, 0b0101);
            cpu.mem_write(0x01, 0x03);
            cpu.mem_write(0x02, 0x07);
            cpu.load_reset(vec![0x31, 0x01, 0x00]);
            cpu.reset();
            cpu.a = 0b1100;
            cpu.y = 0x01;
            cpu.run();
            assert_eq!(cpu.a, 0b0100);
        }
    }

    #[cfg(test)]
    mod asl {
        use super::*;

        #[test]
        fn accumulator() {
            let mut cpu = CPU::new();
            cpu.load_reset(vec![0x0a, 0x00]);
            cpu.a = 0b0101;
            cpu.run();
            assert_eq!(cpu.a, 0b1010);
        }
    }
}
