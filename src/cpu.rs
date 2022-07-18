use std::collections::HashMap;

use log::debug;

use crate::opcodes;
use crate::opcodes::Mnemonic;

const PROGRAM_BEGIN: u16 = 0x0600;
const STACK_BEGIN: u16 = 0x0100;
const SP_BEGIN: u8 = 0xff;
const PC_BEGIN: u16 = 0xfffc;

pub struct CPU {
    pub a: u8,
    pub x: u8,
    pub y: u8,
    pub sp: u8,
    pub status: u8,
    pub pc: u16,
    mem: [u8; 0xFFFF],
    jumped: bool,
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
    Indirect,
    Indirect_X,
    Indirect_Y,
    NoneAddressing,
}

impl AddressingMode {
    fn pc_offset(&self) -> u16 {
        match self {
            AddressingMode::Immediate => 1,
            AddressingMode::ZeroPage => 1,
            AddressingMode::ZeroPage_X => 1,
            AddressingMode::ZeroPage_Y => 1,
            AddressingMode::Absolute => 2,
            AddressingMode::Absolute_X => 2,
            AddressingMode::Absolute_Y => 2,
            AddressingMode::Indirect => 1,
            AddressingMode::Indirect_X => 1,
            AddressingMode::Indirect_Y => 1,
            AddressingMode::NoneAddressing => 0,
        }
    }
}

enum Flag {
    Carry,
    Zero,
    IRQDisabled,
    Decimal,
    Break,
    Reserved,
    OverFlow,
    Negative,
}

#[derive(PartialEq)]
enum Register {
    A,
    X,
    Y,
    S,
    P,
}

impl Flag {
    fn place(&self) -> u8 {
        match self {
            Flag::Carry => 0b0000_0001,
            Flag::Zero => 0b0000_0010,
            Flag::IRQDisabled => 0b0000_0100,
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
            sp: SP_BEGIN,
            status: 0,
            pc: 0,
            mem: [0; 0xFFFF],
            jumped: false,
        }
    }

    pub fn mem_read(&self, addr: u16) -> u8 {
        self.mem[addr as usize]
    }

    pub fn mem_write(&mut self, addr: u16, data: u8) {
        self.mem[addr as usize] = data;
    }

    fn mem_write_with_update_flags(&mut self, addr: u16, data: u8) {
        self.mem[addr as usize] = data;
        self.update_zero_flag(data);
        self.update_negative_flag(data);
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
        self.mem[PROGRAM_BEGIN as usize..(PROGRAM_BEGIN as usize + program.len())].copy_from_slice(&program[..]);
        self.mem_write_u16(PC_BEGIN as u16, PROGRAM_BEGIN);
    }

    fn reset(&mut self) {
        self.a = 0;
        self.x = 0;
        self.status = 0;
        self.pc = self.mem_read_u16(PC_BEGIN);
    }

    pub fn run(&mut self) {
        self.run_with_callback(|_| {});
    }

    pub fn run_with_callback<F>(&mut self, mut callback: F)
        where F: FnMut(&mut CPU), {
        let ref opcodes: HashMap<u8, &'static opcodes::OpCode> = *opcodes::OPCODES_MAP;
        loop {
            callback(self);

            let code = self.mem_read(self.pc);
            self.pc += 1;
            self.jumped = false;

            let opcode = opcodes.get(&code).expect(&format!("OpCode {:x} is not recognized", code));

            debug!("pc: 0x{:x}, code: 0x{:x}, mnemonic: {:?}, mode: {:?}", self.pc, code, opcode.mnemonic, opcode.mode);

            match &(opcode.mnemonic) {
                Mnemonic::LDA => self.ld(&Register::A, &opcode.mode),
                Mnemonic::LDX => self.ld(&Register::X, &opcode.mode),
                Mnemonic::LDY => self.ld(&Register::Y, &opcode.mode),
                Mnemonic::STA => self.st(&Register::A, &opcode.mode),
                Mnemonic::STX => self.st(&Register::X, &opcode.mode),
                Mnemonic::STY => self.st(&Register::Y, &opcode.mode),
                Mnemonic::TAX => self.copy(&Register::A, &Register::X),
                Mnemonic::TAY => self.copy(&Register::A, &Register::Y),
                Mnemonic::TSX => self.copy(&Register::S, &Register::X),
                Mnemonic::TXA => self.copy(&Register::X, &Register::A),
                Mnemonic::TXS => self.copy(&Register::X, &Register::S),
                Mnemonic::TYA => self.copy(&Register::Y, &Register::A),
                Mnemonic::ADC => self.add(&opcode.mode),
                Mnemonic::SBC => self.sub(&opcode.mode),
                Mnemonic::AND => self.and(&opcode.mode),
                Mnemonic::ORA => self.or(&opcode.mode),
                Mnemonic::EOR => self.exclusive_or(&opcode.mode),
                Mnemonic::BIT => self.bit(&opcode.mode),
                Mnemonic::ASL => self.shift_left(&opcode.mode, false),
                Mnemonic::LSR => self.shift_right(&opcode.mode, false),
                Mnemonic::ROL => self.shift_left(&opcode.mode, true),
                Mnemonic::ROR => self.shift_right(&opcode.mode, true),
                Mnemonic::CMP => self.cmp(&Register::A, &opcode.mode),
                Mnemonic::CPX => self.cmp(&Register::X, &opcode.mode),
                Mnemonic::CPY => self.cmp(&Register::Y, &opcode.mode),
                Mnemonic::DEC => self.dec_mem(&opcode.mode),
                Mnemonic::INC => self.inc_mem(&opcode.mode),
                Mnemonic::DEX => self.dec_reg(&Register::X),
                Mnemonic::DEY => self.dec_reg(&Register::Y),
                Mnemonic::INX => self.inc_reg(&Register::X),
                Mnemonic::INY => self.inc_reg(&Register::Y),
                Mnemonic::PHA => self.push(&Register::A),
                Mnemonic::PHP => self.push(&Register::P),
                Mnemonic::PLA => self.pop(&Register::A),
                Mnemonic::PLP => self.pop(&Register::P),
                Mnemonic::JMP => self.jmp(&opcode.mode),
                Mnemonic::JSR => self.jsr(&opcode.mode),
                Mnemonic::RTS => self.rts(),
                Mnemonic::RTI => self.rti(),
                Mnemonic::BCS => self.branch(&Flag::Carry, true, &opcode.mode),
                Mnemonic::BCC => self.branch(&Flag::Carry, false, &opcode.mode),
                Mnemonic::BEQ => self.branch(&Flag::Zero, true, &opcode.mode),
                Mnemonic::BNE => self.branch(&Flag::Zero, false, &opcode.mode),
                Mnemonic::BMI => self.branch(&Flag::Negative, true, &opcode.mode),
                Mnemonic::BPL => self.branch(&Flag::Negative, false, &opcode.mode),
                Mnemonic::BVS => self.branch(&Flag::OverFlow, true, &opcode.mode),
                Mnemonic::BVC => self.branch(&Flag::OverFlow, false, &opcode.mode),
                Mnemonic::CLC => self.set_flag(&Flag::Carry, false),
                Mnemonic::CLD => self.set_flag(&Flag::Decimal, false),
                Mnemonic::CLI => self.set_flag(&Flag::IRQDisabled, false),
                Mnemonic::CLV => self.set_flag(&Flag::OverFlow, false),
                Mnemonic::SEC => self.set_flag(&Flag::Carry, true),
                Mnemonic::SED => self.set_flag(&Flag::Decimal, true),
                Mnemonic::SEI => self.set_flag(&Flag::IRQDisabled, true),
                Mnemonic::BRK => return,
                Mnemonic::NOP => {}
            }
            if !self.jumped {
                self.pc += opcode.mode.pc_offset();
            }
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
            AddressingMode::Indirect => {
                let ptr = self.mem_read(self.pc);
                let lo = self.mem_read(ptr as u16);
                let hi = self.mem_read(ptr.wrapping_add(1) as u16);
                (hi as u16) << 8 | (lo as u16)
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
            Register::P => self.status,
        }
    }

    fn set_register(&mut self, reg: &Register, data: u8) {
        match reg {
            Register::A => self.a = data,
            Register::X => self.x = data,
            Register::Y => self.y = data,
            Register::S => self.sp = data,
            Register::P => self.status = data,
        };
    }

    fn set_register_with_update_flags(&mut self, reg: &Register, data: u8) {
        self.set_register(reg, data);
        self.update_zero_flag(data);
        self.update_negative_flag(data);
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

    fn update_zero_flag(&mut self, result: u8) {
        self.set_flag(&Flag::Zero, result == 0);
    }

    fn update_negative_flag(&mut self, result: u8) {
        self.set_flag(&Flag::Negative, int_type(result) == IntType::Negative);
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

    fn copy(&mut self, from: &Register, to: &Register) {
        let val = self.get_register(from);
        self.set_register_with_update_flags(to, val);
    }

    fn add(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let mem_val = self.mem_read(addr);
        self.add_to_a(mem_val);
    }

    fn sub(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let mem_val = self.mem_read(addr);
        self.add_to_a((mem_val as i8).wrapping_neg().wrapping_sub(1) as u8);
    }

    fn add_to_a(&mut self, param: u8) {
        let carry_val: u8 = if self.get_flag(&Flag::Carry) { 1 } else { 0 };
        let reg_val = self.get_register(&Register::A);
        let (res, over1) = reg_val.overflowing_add(param);
        let (res, over2) = res.overflowing_add(carry_val);
        self.set_flag(&Flag::Carry, over1 || over2);
        self.set_flag(&Flag::OverFlow, (res ^ param) & (res ^ reg_val) & 0b1000_0000 != 0);
        self.set_register_with_update_flags(&Register::A, res);
    }

    fn and(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let mem_val = self.mem_read(addr);
        let val = self.a & mem_val;
        self.set_register_with_update_flags(&Register::A, val);
    }

    fn or(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let mem_val = self.mem_read(addr);
        let reg_val = self.get_register(&Register::A);
        let res = reg_val | mem_val;
        self.set_register_with_update_flags(&Register::A, res);
    }

    fn exclusive_or(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let mem_val = self.mem_read(addr);
        let reg_val = self.get_register(&Register::A);
        let res = reg_val ^ mem_val;
        self.set_register_with_update_flags(&Register::A, res);
    }

    fn bit(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let mem_val = self.mem_read(addr);
        self.set_flag(&Flag::OverFlow, mem_val & 0b0100_0000 > 0);
        self.update_zero_flag(self.a & mem_val);
        self.update_negative_flag(mem_val);
    }

    fn shift_left(&mut self, mode: &AddressingMode, use_carry: bool) {
        let carry_val: u8 = if use_carry && self.get_flag(&Flag::Carry) { 1 } else { 0 };
        match mode {
            AddressingMode::NoneAddressing => {
                let reg_val = self.get_register(&Register::A);
                let res = reg_val.wrapping_shl(1).wrapping_add(carry_val);
                self.set_register_with_update_flags(&Register::A, res);
                self.set_flag(&Flag::Carry, reg_val & 0b1000_0000 != 0);
            }
            _ => {
                let addr = self.get_operand_address(mode);
                let mem_val = self.mem_read(addr);
                let res = mem_val.wrapping_shl(1).wrapping_add(carry_val);
                self.mem_write_with_update_flags(addr, res);
                self.set_flag(&Flag::Carry, mem_val & 0b1000_0000 != 0);
            }
        };
    }

    fn shift_right(&mut self, mode: &AddressingMode, use_carry: bool) {
        let carry_val: u8 = if use_carry && self.get_flag(&Flag::Carry) { 0b1000_0000 } else { 0 };
        match mode {
            AddressingMode::NoneAddressing => {
                let reg_val = self.get_register(&Register::A);
                let res = reg_val.wrapping_shr(1).wrapping_add(carry_val);
                self.set_register_with_update_flags(&Register::A, res);
                self.set_flag(&Flag::Carry, reg_val & 0b0000_0001 != 0);
            }
            _ => {
                let addr = self.get_operand_address(mode);
                let mem_val = self.mem_read(addr).wrapping_add(carry_val);
                let res = mem_val.wrapping_shr(1);
                self.mem_write_with_update_flags(addr, res);
                self.set_flag(&Flag::Carry, mem_val & 0b0000_0001 != 0);
            }
        };
    }

    fn cmp(&mut self, reg: &Register, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let mem_val = self.mem_read(addr);
        let reg_val = self.get_register(reg);
        self.set_flag(&Flag::Carry, reg_val >= mem_val);
        let sub_val = reg_val.wrapping_sub(mem_val);
        self.update_zero_flag(sub_val);
        self.update_negative_flag(sub_val);
    }

    fn dec_mem(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let mem_val = self.mem_read(addr);
        let res = mem_val.wrapping_sub(1);
        self.mem_write_with_update_flags(addr, res);
    }

    fn inc_mem(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let mem_val = self.mem_read(addr);
        let res = mem_val.wrapping_add(1);
        self.mem_write_with_update_flags(addr, res);
    }

    fn dec_reg(&mut self, reg: &Register) {
        let reg_val = self.get_register(reg);
        let res = reg_val.wrapping_sub(1);
        self.set_register_with_update_flags(reg, res);
    }

    fn inc_reg(&mut self, reg: &Register) {
        let reg_val = self.get_register(reg);
        let res = reg_val.wrapping_add(1);
        self.set_register_with_update_flags(reg, res);
    }

    fn push(&mut self, reg: &Register) {
        let reg_val = self.get_register(reg);
        self.stack_push(reg_val);
        if *reg == Register::P {
            self.set_flag(&Flag::Break, true);
            self.set_flag(&Flag::Reserved, true);
        }
    }

    fn pop(&mut self, reg: &Register) {
        let val = self.stack_pop();
        self.set_register(reg, val);
        match *reg {
            Register::A => {
                self.update_zero_flag(val);
                self.update_negative_flag(val);
            }
            Register::P => {
                self.set_flag(&Flag::Break, false);
                self.set_flag(&Flag::Reserved, true);
            }
            _ => {}
        };
    }

    fn stack_push(&mut self, data: u8) {
        let addr = STACK_BEGIN + (self.sp as u16);
        self.mem_write(addr, data);
        self.sp = self.sp.wrapping_sub(1);
    }

    fn stack_pop(&mut self) -> u8 {
        self.sp = self.sp.wrapping_add(1);
        let addr = STACK_BEGIN + (self.sp as u16);
        let val = self.mem_read(addr);
        val
    }

    fn stack_push_u16(&mut self, data: u16) {
        let addr = STACK_BEGIN + (self.sp as u16);
        self.mem_write_u16(addr, data);
        self.sp = self.sp.wrapping_sub(2);
    }

    fn stack_pop_u16(&mut self) -> u16 {
        self.sp = self.sp.wrapping_add(2);
        let addr = STACK_BEGIN + (self.sp as u16);
        let val = self.mem_read_u16(addr);
        val
    }

    fn jmp(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        self.pc = addr;
        self.jumped = true;
    }

    fn jsr(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        self.stack_push_u16(self.pc + (mode.pc_offset() - 1));
        self.pc = addr;
        self.jumped = true;
    }

    fn rts(&mut self) {
        let addr = self.stack_pop_u16() + 1;
        self.pc = addr;
        self.jumped = true;
    }

    fn rti(&mut self) {
        self.status = self.stack_pop();
        self.set_flag(&Flag::Break, false);
        self.set_flag(&Flag::Reserved, true);
        self.pc = self.stack_pop_u16();
        self.jumped = true;
    }

    fn branch(&mut self, flag: &Flag, cond: bool, mode: &AddressingMode) {
        if self.get_flag(flag) != cond {
            return;
        }
        let addr = self.get_operand_address(mode);
        let val = self.mem_read(addr);
        self.pc = self.pc.wrapping_add(val as u16).wrapping_add(1);
        self.jumped = true;
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
        fn accumulator_no_carry() {
            let mut cpu = CPU::new();
            cpu.load_reset(vec![0x0a, 0x00]);
            cpu.a = 0b0101;
            cpu.run();
            assert_eq!(cpu.a, 0b1010);
            assert_eq!(cpu.get_flag(&Flag::Carry), false);
        }

        #[test]
        fn accumulator_with_carry() {
            let mut cpu = CPU::new();
            cpu.load_reset(vec![0x0a, 0x00]);
            cpu.a = 0b1010_1010;
            cpu.run();
            assert_eq!(cpu.a, 0b0101_0100);
            assert_eq!(cpu.get_flag(&Flag::Carry), true);
        }

        #[test]
        fn zeropage() {
            let mut cpu = CPU::new();
            cpu.mem_write(0x10, 0b0101);
            cpu.load_reset(vec![0x06, 0x10, 0x00]);
            cpu.a = 0b1010;
            cpu.run();
            assert_eq!(cpu.mem_read(0x10), 0b1010);
        }

        #[test]
        fn zeropage_x() {
            let mut cpu = CPU::new();
            cpu.mem_write(0x11, 0b0101);
            cpu.load_reset(vec![0x16, 0x10, 0x00]);
            cpu.reset();
            cpu.x = 0x01;
            cpu.run();
            assert_eq!(cpu.mem_read(0x11), 0b1010);
        }

        #[test]
        fn absolute() {
            let mut cpu = CPU::new();
            cpu.mem_write(0x1122, 0b0101);
            cpu.load_reset(vec![0x0e, 0x22, 0x11, 0x00]);
            cpu.run();
            assert_eq!(cpu.mem_read(0x1122), 0b1010);
        }

        #[test]
        fn absolute_x() {
            let mut cpu = CPU::new();
            cpu.mem_write(0x1133, 0b0101);
            cpu.load_reset(vec![0x1e, 0x22, 0x11, 0x00]);
            cpu.reset();
            cpu.x = 0x11;
            cpu.run();
            assert_eq!(cpu.mem_read(0x1133), 0b1010);
        }
    }

    #[cfg(test)]
    mod lsr {
        use super::*;

        #[test]
        fn accumulator_no_carry() {
            let mut cpu = CPU::new();
            cpu.load_reset(vec![0x4a, 0x00]);
            cpu.a = 0b1010_1010;
            cpu.run();
            assert_eq!(cpu.a, 0b0101_0101);
            assert_eq!(cpu.get_flag(&Flag::Carry), false);
        }

        #[test]
        fn accumulator_with_carry() {
            let mut cpu = CPU::new();
            cpu.load_reset(vec![0x4a, 0x00]);
            cpu.a = 0b0101_0101;
            cpu.run();
            assert_eq!(cpu.a, 0b0010_1010);
            assert_eq!(cpu.get_flag(&Flag::Carry), true);
        }

        #[test]
        fn zeropage() {
            let mut cpu = CPU::new();
            cpu.mem_write(0x10, 0b1010);
            cpu.load_reset(vec![0x46, 0x10, 0x00]);
            cpu.run();
            assert_eq!(cpu.mem_read(0x10), 0b0101);
        }

        #[test]
        fn zeropage_x() {
            let mut cpu = CPU::new();
            cpu.mem_write(0x11, 0b1010);
            cpu.load_reset(vec![0x56, 0x10, 0x00]);
            cpu.reset();
            cpu.x = 0x01;
            cpu.run();
            assert_eq!(cpu.mem_read(0x11), 0b0101);
        }

        #[test]
        fn absolute() {
            let mut cpu = CPU::new();
            cpu.mem_write(0x1122, 0b1010);
            cpu.load_reset(vec![0x4e, 0x22, 0x11, 0x00]);
            cpu.run();
            assert_eq!(cpu.mem_read(0x1122), 0b0101);
        }

        #[test]
        fn absolute_x() {
            let mut cpu = CPU::new();
            cpu.mem_write(0x1133, 0b1010);
            cpu.load_reset(vec![0x5e, 0x22, 0x11, 0x00]);
            cpu.reset();
            cpu.x = 0x11;
            cpu.run();
            assert_eq!(cpu.mem_read(0x1133), 0b0101);
        }
    }

    #[cfg(test)]
    mod rol {
        use super::*;

        #[test]
        fn accumulator_has_no_carry_set_carry() {
            let mut cpu = CPU::new();
            cpu.load_reset(vec![0x2a, 0x00]);
            cpu.a = 0b1010_1010;
            cpu.run();
            assert_eq!(cpu.a, 0b0101_0100);
            assert_eq!(cpu.get_flag(&Flag::Carry), true);
        }

        #[test]
        fn accumulator_has_carry_unset_carry() {
            let mut cpu = CPU::new();
            cpu.load_reset(vec![0x2a, 0x00]);
            cpu.a = 0b0010_1010;
            cpu.set_flag(&Flag::Carry, true);
            cpu.run();
            assert_eq!(cpu.a, 0b0101_0101);
            assert_eq!(cpu.get_flag(&Flag::Carry), false);
        }

        #[test]
        fn zeropage() {
            let mut cpu = CPU::new();
            cpu.mem_write(0x10, 0b0101);
            cpu.load_reset(vec![0x26, 0x10, 0x00]);
            cpu.run();
            assert_eq!(cpu.mem_read(0x10), 0b1010);
        }

        #[test]
        fn zeropage_x() {
            let mut cpu = CPU::new();
            cpu.mem_write(0x11, 0b0101);
            cpu.load_reset(vec![0x36, 0x10, 0x00]);
            cpu.reset();
            cpu.x = 0x01;
            cpu.run();
            assert_eq!(cpu.mem_read(0x11), 0b1010);
        }

        #[test]
        fn absolute() {
            let mut cpu = CPU::new();
            cpu.mem_write(0x1122, 0b0101);
            cpu.load_reset(vec![0x2e, 0x22, 0x11, 0x00]);
            cpu.run();
            assert_eq!(cpu.mem_read(0x1122), 0b1010);
        }

        #[test]
        fn absolute_x() {
            let mut cpu = CPU::new();
            cpu.mem_write(0x1133, 0b0101);
            cpu.load_reset(vec![0x3e, 0x22, 0x11, 0x00]);
            cpu.reset();
            cpu.x = 0x11;
            cpu.run();
            assert_eq!(cpu.mem_read(0x1133), 0b1010);
        }
    }

    #[cfg(test)]
    mod ror {
        use super::*;

        #[test]
        fn accumulator_has_no_carry_set_carry() {
            let mut cpu = CPU::new();
            cpu.load_reset(vec![0x6a, 0x00]);
            cpu.a = 0b0101_0101;
            cpu.run();
            assert_eq!(cpu.a, 0b0010_1010);
            assert_eq!(cpu.get_flag(&Flag::Carry), true);
        }

        #[test]
        fn accumulator_has_carry_unset_carry() {
            let mut cpu = CPU::new();
            cpu.load_reset(vec![0x6a, 0x00]);
            cpu.a = 0b0101_0100;
            cpu.set_flag(&Flag::Carry, true);
            cpu.run();
            assert_eq!(cpu.a, 0b1010_1010);
            assert_eq!(cpu.get_flag(&Flag::Carry), false);
        }

        #[test]
        fn zeropage() {
            let mut cpu = CPU::new();
            cpu.mem_write(0x10, 0b1010);
            cpu.load_reset(vec![0x66, 0x10, 0x00]);
            cpu.run();
            assert_eq!(cpu.mem_read(0x10), 0b0101);
        }

        #[test]
        fn zeropage_x() {
            let mut cpu = CPU::new();
            cpu.mem_write(0x11, 0b1010);
            cpu.load_reset(vec![0x76, 0x10, 0x00]);
            cpu.reset();
            cpu.x = 0x01;
            cpu.run();
            assert_eq!(cpu.mem_read(0x11), 0b0101);
        }

        #[test]
        fn absolute() {
            let mut cpu = CPU::new();
            cpu.mem_write(0x1122, 0b1010);
            cpu.load_reset(vec![0x6e, 0x22, 0x11, 0x00]);
            cpu.run();
            assert_eq!(cpu.mem_read(0x1122), 0b0101);
        }

        #[test]
        fn absolute_x() {
            let mut cpu = CPU::new();
            cpu.mem_write(0x1133, 0b1010);
            cpu.load_reset(vec![0x7e, 0x22, 0x11, 0x00]);
            cpu.reset();
            cpu.x = 0x11;
            cpu.run();
            assert_eq!(cpu.mem_read(0x1133), 0b0101);
        }
    }

    #[cfg(test)]
    mod bit {
        use super::*;

        #[test]
        fn zeropage_zero() {
            let mut cpu = CPU::new();
            cpu.mem_write(0x10, 0b_0000_0101);
            cpu.load_reset(vec![0x24, 0x10, 0x00]);
            cpu.a = 0b0000_1010;
            cpu.run();
            assert_eq!(cpu.get_flag(&Flag::Zero), true);
            assert_eq!(cpu.get_flag(&Flag::OverFlow), false);
            assert_eq!(cpu.get_flag(&Flag::Negative), false);
        }

        #[test]
        fn zeropage_overflow() {
            let mut cpu = CPU::new();
            cpu.mem_write(0x10, 0b_0100_0101);
            cpu.load_reset(vec![0x24, 0x10, 0x00]);
            cpu.a = 0b0000_0101;
            cpu.run();
            assert_eq!(cpu.get_flag(&Flag::Zero), false);
            assert_eq!(cpu.get_flag(&Flag::OverFlow), true);
            assert_eq!(cpu.get_flag(&Flag::Negative), false);
        }

        #[test]
        fn zeropage_negative() {
            let mut cpu = CPU::new();
            cpu.mem_write(0x10, 0b_1000_0101);
            cpu.load_reset(vec![0x24, 0x10, 0x00]);
            cpu.a = 0b0000_0101;
            cpu.run();
            assert_eq!(cpu.get_flag(&Flag::Zero), false);
            assert_eq!(cpu.get_flag(&Flag::OverFlow), false);
            assert_eq!(cpu.get_flag(&Flag::Negative), true);
        }

        #[test]
        fn absolute_zero() {
            let mut cpu = CPU::new();
            cpu.mem_write(0x1122, 0b_0000_0101);
            cpu.load_reset(vec![0x2c, 0x22, 0x11, 0x00]);
            cpu.a = 0b0000_1010;
            cpu.run();
            assert_eq!(cpu.get_flag(&Flag::Zero), true);
            assert_eq!(cpu.get_flag(&Flag::OverFlow), false);
            assert_eq!(cpu.get_flag(&Flag::Negative), false);
        }

        #[test]
        fn absolute_overflow() {
            let mut cpu = CPU::new();
            cpu.mem_write(0x1122, 0b_0100_0101);
            cpu.load_reset(vec![0x2c, 0x22, 0x11, 0x00]);
            cpu.a = 0b0000_0101;
            cpu.run();
            assert_eq!(cpu.get_flag(&Flag::Zero), false);
            assert_eq!(cpu.get_flag(&Flag::OverFlow), true);
            assert_eq!(cpu.get_flag(&Flag::Negative), false);
        }

        #[test]
        fn absolute_negative() {
            let mut cpu = CPU::new();
            cpu.mem_write(0x1122, 0b_1000_0101);
            cpu.load_reset(vec![0x2c, 0x22, 0x11, 0x00]);
            cpu.a = 0b0000_0101;
            cpu.run();
            assert_eq!(cpu.get_flag(&Flag::Zero), false);
            assert_eq!(cpu.get_flag(&Flag::OverFlow), false);
            assert_eq!(cpu.get_flag(&Flag::Negative), true);
        }
    }

    #[cfg(test)]
    mod cmp {
        use super::*;

        #[test]
        fn immediate_greater_than_param() {
            let mut cpu = CPU::new();
            cpu.load_reset(vec![0xc9, 0x11, 0x00]);
            cpu.a = 0x22;
            cpu.run();
            assert_eq!(cpu.get_flag(&Flag::Carry), true);
            assert_eq!(cpu.get_flag(&Flag::Zero), false);
            assert_eq!(cpu.get_flag(&Flag::Negative), false);
        }

        #[test]
        fn immediate_equal_with_param() {
            let mut cpu = CPU::new();
            cpu.load_reset(vec![0xc9, 0x22, 0x00]);
            cpu.a = 0x22;
            cpu.run();
            assert_eq!(cpu.get_flag(&Flag::Carry), true);
            assert_eq!(cpu.get_flag(&Flag::Zero), true);
            assert_eq!(cpu.get_flag(&Flag::Negative), false);
        }

        #[test]
        fn immediate_less_than_param() {
            let mut cpu = CPU::new();
            cpu.load_reset(vec![0xc9, 0x33, 0x00]);
            cpu.a = 0x22;
            cpu.run();
            assert_eq!(cpu.get_flag(&Flag::Carry), false);
            assert_eq!(cpu.get_flag(&Flag::Zero), false);
            assert_eq!(cpu.get_flag(&Flag::Negative), true);
        }

        #[test]
        fn zeropage() {
            let mut cpu = CPU::new();
            cpu.mem_write(0x10, 0x11);
            cpu.load_reset(vec![0xc5, 0x10, 0x00]);
            cpu.a = 0x22;
            cpu.run();
            assert_eq!(cpu.get_flag(&Flag::Carry), true);
        }

        #[test]
        fn zeropage_x() {
            let mut cpu = CPU::new();
            cpu.mem_write(0x11, 0x11);
            cpu.load_reset(vec![0xd5, 0x10, 0x00]);
            cpu.a = 0x22;
            cpu.x = 0x01;
            cpu.run();
            assert_eq!(cpu.get_flag(&Flag::Carry), true);
        }

        #[test]
        fn absolute() {
            let mut cpu = CPU::new();
            cpu.mem_write(0x1122, 0x11);
            cpu.load_reset(vec![0xcd, 0x22, 0x11, 0x00]);
            cpu.a = 0x22;
            cpu.run();
            assert_eq!(cpu.get_flag(&Flag::Carry), true);
        }

        #[test]
        fn absolute_x() {
            let mut cpu = CPU::new();
            cpu.mem_write(0x1133, 0x11);
            cpu.load_reset(vec![0xdd, 0x22, 0x11, 0x00]);
            cpu.reset();
            cpu.a = 0x22;
            cpu.x = 0x11;
            cpu.run();
            assert_eq!(cpu.get_flag(&Flag::Carry), true);
        }

        #[test]
        fn absolute_y() {
            let mut cpu = CPU::new();
            cpu.mem_write(0x1133, 0x11);
            cpu.load_reset(vec![0xd9, 0x22, 0x11, 0x00]);
            cpu.reset();
            cpu.a = 0x22;
            cpu.y = 0x11;
            cpu.run();
            assert_eq!(cpu.get_flag(&Flag::Carry), true);
        }

        #[test]
        fn indirect_x() {
            let mut cpu = CPU::new();
            cpu.mem_write(0x0705, 0x11);
            cpu.mem_write(0x01, 0x05);
            cpu.mem_write(0x02, 0x07);
            cpu.load_reset(vec![0xc1, 0x00, 0x00]);
            cpu.reset();
            cpu.a = 0x22;
            cpu.x = 0x01;
            cpu.run();
            assert_eq!(cpu.get_flag(&Flag::Carry), true);
        }

        #[test]
        fn indirect_y() {
            let mut cpu = CPU::new();
            cpu.mem_write(0x0704, 0x11);
            cpu.mem_write(0x01, 0x03);
            cpu.mem_write(0x02, 0x07);
            cpu.load_reset(vec![0xd1, 0x01, 0x00]);
            cpu.reset();
            cpu.a = 0x22;
            cpu.y = 0x01;
            cpu.run();
            assert_eq!(cpu.get_flag(&Flag::Carry), true);
        }
    }

    #[cfg(test)]
    mod cpx {
        use super::*;

        #[test]
        fn immediate() {
            let mut cpu = CPU::new();
            cpu.load_reset(vec![0xe0, 0x11, 0x00]);
            cpu.x = 0x22;
            cpu.run();
            assert_eq!(cpu.get_flag(&Flag::Carry), true);
        }

        #[test]
        fn zeropage() {
            let mut cpu = CPU::new();
            cpu.mem_write(0x10, 0x11);
            cpu.load_reset(vec![0xe4, 0x10, 0x00]);
            cpu.x = 0x22;
            cpu.run();
            assert_eq!(cpu.get_flag(&Flag::Carry), true);
        }

        #[test]
        fn absolute() {
            let mut cpu = CPU::new();
            cpu.mem_write(0x1122, 0x11);
            cpu.load_reset(vec![0xec, 0x22, 0x11, 0x00]);
            cpu.x = 0x22;
            cpu.run();
            assert_eq!(cpu.get_flag(&Flag::Carry), true);
        }
    }

    #[cfg(test)]
    mod cpy {
        use super::*;

        #[test]
        fn immediate() {
            let mut cpu = CPU::new();
            cpu.load_reset(vec![0xc0, 0x11, 0x00]);
            cpu.y = 0x22;
            cpu.run();
            assert_eq!(cpu.get_flag(&Flag::Carry), true);
        }

        #[test]
        fn zeropage() {
            let mut cpu = CPU::new();
            cpu.mem_write(0x10, 0x11);
            cpu.load_reset(vec![0xc4, 0x10, 0x00]);
            cpu.y = 0x22;
            cpu.run();
            assert_eq!(cpu.get_flag(&Flag::Carry), true);
        }

        #[test]
        fn absolute() {
            let mut cpu = CPU::new();
            cpu.mem_write(0x1122, 0x11);
            cpu.load_reset(vec![0xcc, 0x22, 0x11, 0x00]);
            cpu.y = 0x22;
            cpu.run();
            assert_eq!(cpu.get_flag(&Flag::Carry), true);
        }
    }

    #[cfg(test)]
    mod dec {
        use super::*;

        #[test]
        fn zeropage() {
            let mut cpu = CPU::new();
            cpu.mem_write(0x10, 0x11);
            cpu.load_reset_run(vec![0xc6, 0x10, 0x00]);
            assert_eq!(cpu.mem_read(0x10), 0x10);
        }

        #[test]
        fn zeropage_x() {
            let mut cpu = CPU::new();
            cpu.mem_write(0x11, 0x11);
            cpu.load_reset(vec![0xd6, 0x10, 0x00]);
            cpu.x = 0x01;
            cpu.run();
            assert_eq!(cpu.mem_read(0x11), 0x10);
        }

        #[test]
        fn absolute() {
            let mut cpu = CPU::new();
            cpu.mem_write(0x1122, 0x11);
            cpu.load_reset_run(vec![0xce, 0x22, 0x11, 0x00]);
            assert_eq!(cpu.mem_read(0x1122), 0x10);
        }

        #[test]
        fn absolute_x() {
            let mut cpu = CPU::new();
            cpu.mem_write(0x1133, 0x11);
            cpu.load_reset(vec![0xde, 0x22, 0x11, 0x00]);
            cpu.x = 0x11;
            cpu.run();
            assert_eq!(cpu.mem_read(0x1133), 0x10);
        }
    }

    #[cfg(test)]
    mod inc {
        use super::*;

        #[test]
        fn zeropage() {
            let mut cpu = CPU::new();
            cpu.mem_write(0x10, 0x11);
            cpu.load_reset_run(vec![0xe6, 0x10, 0x00]);
            assert_eq!(cpu.mem_read(0x10), 0x12);
        }

        #[test]
        fn zeropage_x() {
            let mut cpu = CPU::new();
            cpu.mem_write(0x11, 0x11);
            cpu.load_reset(vec![0xf6, 0x10, 0x00]);
            cpu.x = 0x01;
            cpu.run();
            assert_eq!(cpu.mem_read(0x11), 0x12);
        }

        #[test]
        fn absolute() {
            let mut cpu = CPU::new();
            cpu.mem_write(0x1122, 0x11);
            cpu.load_reset_run(vec![0xee, 0x22, 0x11, 0x00]);
            assert_eq!(cpu.mem_read(0x1122), 0x12);
        }

        #[test]
        fn absolute_x() {
            let mut cpu = CPU::new();
            cpu.mem_write(0x1133, 0x11);
            cpu.load_reset(vec![0xfe, 0x22, 0x11, 0x00]);
            cpu.x = 0x11;
            cpu.run();
            assert_eq!(cpu.mem_read(0x1133), 0x12);
        }
    }

    #[test]
    fn dex() {
        let mut cpu = CPU::new();
        cpu.load_reset(vec![0xca, 0x00]);
        cpu.x = 0x11;
        cpu.run();
        assert_eq!(cpu.x, 0x10)
    }

    #[test]
    fn dey() {
        let mut cpu = CPU::new();
        cpu.load_reset(vec![0x88, 0x00]);
        cpu.y = 0x11;
        cpu.run();
        assert_eq!(cpu.y, 0x10)
    }

    #[test]
    fn inx() {
        let mut cpu = CPU::new();
        cpu.load_reset(vec![0xe8, 0x00]);
        cpu.x = 0x11;
        cpu.run();
        assert_eq!(cpu.x, 0x12)
    }

    #[test]
    fn iny() {
        let mut cpu = CPU::new();
        cpu.load_reset(vec![0xc8, 0x00]);
        cpu.y = 0x11;
        cpu.run();
        assert_eq!(cpu.y, 0x12)
    }

    #[cfg(test)]
    mod eor {
        use super::*;

        #[test]
        fn immediate() {
            let mut cpu = CPU::new();
            cpu.load_reset(vec![0x49, 0b0101, 0x00]);
            cpu.a = 0b1100;
            cpu.run();
            assert_eq!(cpu.a, 0b1001);
        }

        #[test]
        fn zeropage() {
            let mut cpu = CPU::new();
            cpu.mem_write(0x10, 0b0101);
            cpu.load_reset(vec![0x45, 0x10, 0x00]);
            cpu.a = 0b1100;
            cpu.run();
            assert_eq!(cpu.a, 0b1001);
        }

        #[test]
        fn zeropage_x() {
            let mut cpu = CPU::new();
            cpu.mem_write(0x11, 0b0101);
            cpu.load_reset(vec![0x55, 0x10, 0x00]);
            cpu.a = 0b1100;
            cpu.x = 0x01;
            cpu.run();
            assert_eq!(cpu.a, 0b1001);
        }

        #[test]
        fn absolute() {
            let mut cpu = CPU::new();
            cpu.mem_write(0x1122, 0b0101);
            cpu.load_reset(vec![0x4d, 0x22, 0x11, 0x00]);
            cpu.a = 0b1100;
            cpu.run();
            assert_eq!(cpu.a, 0b1001);
        }

        #[test]
        fn absolute_x() {
            let mut cpu = CPU::new();
            cpu.mem_write(0x1133, 0b0101);
            cpu.load_reset(vec![0x5d, 0x22, 0x11, 0x00]);
            cpu.a = 0b1100;
            cpu.x = 0x11;
            cpu.run();
            assert_eq!(cpu.a, 0b1001);
        }

        #[test]
        fn absolute_y() {
            let mut cpu = CPU::new();
            cpu.mem_write(0x1133, 0b0101);
            cpu.load_reset(vec![0x59, 0x22, 0x11, 0x00]);
            cpu.a = 0b1100;
            cpu.y = 0x11;
            cpu.run();
            assert_eq!(cpu.a, 0b1001);
        }

        #[test]
        fn indirect_x() {
            let mut cpu = CPU::new();
            cpu.mem_write(0x0705, 0b0101);
            cpu.mem_write(0x01, 0x05);
            cpu.mem_write(0x02, 0x07);
            cpu.load_reset(vec![0x41, 0x00, 0x00]);
            cpu.a = 0b1100;
            cpu.x = 0x01;
            cpu.run();
            assert_eq!(cpu.a, 0b1001);
        }

        #[test]
        fn indirect_y() {
            let mut cpu = CPU::new();
            cpu.mem_write(0x0704, 0b0101);
            cpu.mem_write(0x01, 0x03);
            cpu.mem_write(0x02, 0x07);
            cpu.load_reset(vec![0x51, 0x01, 0x00]);
            cpu.a = 0b1100;
            cpu.y = 0x01;
            cpu.run();
            assert_eq!(cpu.a, 0b1001);
        }
    }

    #[cfg(test)]
    mod ora {
        use super::*;

        #[test]
        fn immediate() {
            let mut cpu = CPU::new();
            cpu.load_reset(vec![0x09, 0b0101, 0x00]);
            cpu.a = 0b1100;
            cpu.run();
            assert_eq!(cpu.a, 0b1101);
        }

        #[test]
        fn zeropage() {
            let mut cpu = CPU::new();
            cpu.mem_write(0x10, 0b0101);
            cpu.load_reset(vec![0x05, 0x10, 0x00]);
            cpu.a = 0b1100;
            cpu.run();
            assert_eq!(cpu.a, 0b1101);
        }

        #[test]
        fn zeropage_x() {
            let mut cpu = CPU::new();
            cpu.mem_write(0x11, 0b0101);
            cpu.load_reset(vec![0x15, 0x10, 0x00]);
            cpu.a = 0b1100;
            cpu.x = 0x01;
            cpu.run();
            assert_eq!(cpu.a, 0b1101);
        }

        #[test]
        fn absolute() {
            let mut cpu = CPU::new();
            cpu.mem_write(0x1122, 0b0101);
            cpu.load_reset(vec![0x0d, 0x22, 0x11, 0x00]);
            cpu.a = 0b1100;
            cpu.run();
            assert_eq!(cpu.a, 0b1101);
        }

        #[test]
        fn absolute_x() {
            let mut cpu = CPU::new();
            cpu.mem_write(0x1133, 0b0101);
            cpu.load_reset(vec![0x1d, 0x22, 0x11, 0x00]);
            cpu.a = 0b1100;
            cpu.x = 0x11;
            cpu.run();
            assert_eq!(cpu.a, 0b1101);
        }

        #[test]
        fn absolute_y() {
            let mut cpu = CPU::new();
            cpu.mem_write(0x1133, 0b0101);
            cpu.load_reset(vec![0x19, 0x22, 0x11, 0x00]);
            cpu.a = 0b1100;
            cpu.y = 0x11;
            cpu.run();
            assert_eq!(cpu.a, 0b1101);
        }

        #[test]
        fn indirect_x() {
            let mut cpu = CPU::new();
            cpu.mem_write(0x0705, 0b0101);
            cpu.mem_write(0x01, 0x05);
            cpu.mem_write(0x02, 0x07);
            cpu.load_reset(vec![0x01, 0x00, 0x00]);
            cpu.a = 0b1100;
            cpu.x = 0x01;
            cpu.run();
            assert_eq!(cpu.a, 0b1101);
        }

        #[test]
        fn indirect_y() {
            let mut cpu = CPU::new();
            cpu.mem_write(0x0704, 0b0101);
            cpu.mem_write(0x01, 0x03);
            cpu.mem_write(0x02, 0x07);
            cpu.load_reset(vec![0x11, 0x01, 0x00]);
            cpu.a = 0b1100;
            cpu.y = 0x01;
            cpu.run();
            assert_eq!(cpu.a, 0b1101);
        }
    }

    #[test]
    fn pha() {
        let mut cpu = CPU::new();
        cpu.load_reset(vec![0x48, 0x00]);
        cpu.a = 0xaa;
        cpu.run();
        assert_eq!(cpu.mem_read(0x01ff), 0xaa);
        assert_eq!(cpu.sp, 0xfe);
    }

    #[test]
    fn php() {
        let mut cpu = CPU::new();
        cpu.load_reset(vec![0x08, 0x00]);
        cpu.status = 0xaa;
        cpu.run();
        assert_eq!(cpu.mem_read(0x01ff), 0xaa);
        assert_eq!(cpu.sp, 0xfe);
        assert_eq!(cpu.get_flag(&Flag::Break), true);
        assert_eq!(cpu.get_flag(&Flag::Reserved), true);
    }

    #[test]
    fn pla() {
        let mut cpu = CPU::new();
        cpu.mem_write(0x01bb, 0xaa);
        cpu.load_reset(vec![0x68, 0x00]);
        cpu.sp = 0xba;
        cpu.run();
        assert_eq!(cpu.a, 0xaa);
        assert_eq!(cpu.sp, 0xbb);
    }

    #[test]
    fn plp() {
        let mut cpu = CPU::new();
        cpu.mem_write(0x01bb, 0b1101_1111);
        cpu.load_reset(vec![0x28, 0x00]);
        cpu.sp = 0xba;
        cpu.run();
        assert_eq!(cpu.status, 0b1110_1111); // Break=false, Reserved=true
        assert_eq!(cpu.sp, 0xbb);
    }

    #[cfg(test)]
    mod jmp {
        use super::*;

        #[test]
        fn absolute() {
            let mut cpu = CPU::new();
            cpu.load_reset_run(vec![0x4c, 0x22, 0x11, 0x00]);
            assert_eq!(cpu.pc, 0x1123);
        }

        #[test]
        fn indirect() {
            let mut cpu = CPU::new();
            cpu.mem_write_u16(0x11, 0x3344);
            cpu.load_reset_run(vec![0x6c, 0x11, 0x00]);
            assert_eq!(cpu.pc, 0x3345);
        }
    }

    #[test]
    fn jsr() {
        let mut cpu = CPU::new();
        cpu.load_reset_run(vec![0x20, 0x22, 0x11, 0x00]);
        assert_eq!(cpu.pc, 0x1123);
        assert_eq!(cpu.mem_read_u16(0x01ff), PROGRAM_BEGIN + 0x0002);
    }

    #[cfg(test)]
    mod rts {
        use super::*;

        #[test]
        fn implied() {
            let mut cpu = CPU::new();
            cpu.load_reset(vec![0x60]);
            cpu.stack_push_u16(0x1122);
            cpu.run();
            assert_eq!(cpu.pc, 0x1124);
            assert_eq!(cpu.sp, SP_BEGIN);
        }

        #[test]
        fn implied_with_jsr() {
            let mut cpu = CPU::new();
            // (PROGRAM_BEGIN=$8000)
            // JSR $1122 ; $8000
            // LDA #$AA  ; $8003
            // BRK       ; $8005
            // LDX #$BB  ; $8006
            // RTS       ; $8008
            // cpu.mem_write_u16(0x1122, PROGRAM_BEGIN + 0x0006);
            let jump_to = PROGRAM_BEGIN + 0x0006;
            let jump_to_hi = (jump_to >> 8) as u8;
            let jump_to_lo = jump_to as u8;
            cpu.load_reset_run(vec![0x20, jump_to_lo, jump_to_hi, 0xa9, 0xaa, 0x00, 0xa2, 0xbb, 0x60]);
            assert_eq!(cpu.a, 0xaa);
            assert_eq!(cpu.x, 0xbb);
        }
    }

    #[test]
    fn rti() {
        let mut cpu = CPU::new();
        cpu.load_reset(vec![0x40]);
        cpu.stack_push_u16(0x1122);
        cpu.stack_push(0b1101_1111);
        cpu.run();
        assert_eq!(cpu.pc, 0x1123);
        assert_eq!(cpu.status, 0b1110_1111);
    }

    #[cfg(test)]
    mod bcc {
        use super::*;

        #[test]
        fn immediate_forward() {
            let mut cpu = CPU::new();
            //   BCC label
            //   BRK
            // label:
            //   LDA #$aa
            //   BRK
            cpu.load_reset(vec![0x90, 0x01, 0x00, 0xa9, 0xaa, 0x00]);
            cpu.set_flag(&Flag::Carry, false);
            cpu.run();
            assert_eq!(cpu.a, 0xaa);
        }
    }

    #[test]
    fn bcs() {
        let mut cpu = CPU::new();
        //   BCS label
        //   BRK
        // label:
        //   LDA #$aa
        //   BRK
        cpu.load_reset(vec![0xb0, 0x01, 0x00, 0xa9, 0xaa, 0x00]);
        cpu.set_flag(&Flag::Carry, true);
        cpu.run();
        assert_eq!(cpu.a, 0xaa);
    }

    #[test]
    fn beq() {
        let mut cpu = CPU::new();
        //   BEQ label
        //   BRK
        // label:
        //   LDA #$aa
        //   BRK
        cpu.load_reset(vec![0xf0, 0x01, 0x00, 0xa9, 0xaa, 0x00]);
        cpu.set_flag(&Flag::Zero, true);
        cpu.run();
        assert_eq!(cpu.a, 0xaa);
    }

    #[test]
    fn bmi() {
        let mut cpu = CPU::new();
        //   BMI label
        //   BRK
        // label:
        //   LDA #$aa
        //   BRK
        cpu.load_reset(vec![0x30, 0x01, 0x00, 0xa9, 0xaa, 0x00]);
        cpu.set_flag(&Flag::Negative, true);
        cpu.run();
        assert_eq!(cpu.a, 0xaa);
    }

    #[test]
    fn bne() {
        let mut cpu = CPU::new();
        //   BNE label
        //   BRK
        // label:
        //   LDA #$aa
        //   BRK
        cpu.load_reset(vec![0xd0, 0x01, 0x00, 0xa9, 0xaa, 0x00]);
        cpu.set_flag(&Flag::Zero, false);
        cpu.run();
        assert_eq!(cpu.a, 0xaa);
    }

    #[test]
    fn bpl() {
        let mut cpu = CPU::new();
        //   BPL label
        //   BRK
        // label:
        //   LDA #$aa
        //   BRK
        cpu.load_reset(vec![0x10, 0x01, 0x00, 0xa9, 0xaa, 0x00]);
        cpu.set_flag(&Flag::Negative, false);
        cpu.run();
        assert_eq!(cpu.a, 0xaa);
    }

    #[test]
    fn bvc() {
        let mut cpu = CPU::new();
        //   BVC label
        //   BRK
        // label:
        //   LDA #$aa
        //   BRK
        cpu.load_reset(vec![0x50, 0x01, 0x00, 0xa9, 0xaa, 0x00]);
        cpu.set_flag(&Flag::OverFlow, false);
        cpu.run();
        assert_eq!(cpu.a, 0xaa);
    }

    #[test]
    fn bvs() {
        let mut cpu = CPU::new();
        //   BVS label
        //   BRK
        // label:
        //   LDA #$aa
        //   BRK
        cpu.load_reset(vec![0x70, 0x01, 0x00, 0xa9, 0xaa, 0x00]);
        cpu.set_flag(&Flag::OverFlow, true);
        cpu.run();
        assert_eq!(cpu.a, 0xaa);
    }

    #[test]
    fn clc() {
        let mut cpu = CPU::new();
        cpu.load_reset(vec![0x18, 0x00]);
        cpu.set_flag(&Flag::Carry, true);
        cpu.run();
        assert_eq!(cpu.get_flag(&Flag::Carry), false);
    }

    #[test]
    fn cld() {
        let mut cpu = CPU::new();
        cpu.load_reset(vec![0xd8, 0x00]);
        cpu.set_flag(&Flag::Decimal, true);
        cpu.run();
        assert_eq!(cpu.get_flag(&Flag::Decimal), false);
    }

    #[test]
    fn cli() {
        let mut cpu = CPU::new();
        cpu.load_reset(vec![0x58, 0x00]);
        cpu.set_flag(&Flag::IRQDisabled, true);
        cpu.run();
        assert_eq!(cpu.get_flag(&Flag::IRQDisabled), false);
    }

    #[test]
    fn clv() {
        let mut cpu = CPU::new();
        cpu.load_reset(vec![0xb8, 0x00]);
        cpu.set_flag(&Flag::OverFlow, true);
        cpu.run();
        assert_eq!(cpu.get_flag(&Flag::OverFlow), false);
    }

    #[test]
    fn sec() {
        let mut cpu = CPU::new();
        cpu.load_reset(vec![0x38, 0x00]);
        cpu.set_flag(&Flag::Carry, false);
        cpu.run();
        assert_eq!(cpu.get_flag(&Flag::Carry), true);
    }

    #[test]
    fn sed() {
        let mut cpu = CPU::new();
        cpu.load_reset(vec![0xf8, 0x00]);
        cpu.set_flag(&Flag::Decimal, false);
        cpu.run();
        assert_eq!(cpu.get_flag(&Flag::Decimal), true);
    }

    #[test]
    fn sei() {
        let mut cpu = CPU::new();
        cpu.load_reset(vec![0x78, 0x00]);
        cpu.set_flag(&Flag::IRQDisabled, false);
        cpu.run();
        assert_eq!(cpu.get_flag(&Flag::IRQDisabled), true);
    }

    #[test]
    fn brk() {
        let mut cpu = CPU::new();
        cpu.load_reset_run(vec![0x00]);
    }

    #[test]
    fn nop() {
        let mut cpu = CPU::new();
        cpu.load_reset_run(vec![0xea, 0x00]);
    }
}