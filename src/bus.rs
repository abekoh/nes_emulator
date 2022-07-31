use log::debug;

use crate::Mem;
use crate::rom::Rom;

pub struct Bus {
    cpu_vram: [u8; 2048],
    rom: Rom,
}

const RAM: u16 = 0x0000;
const RAM_MIRRORS_END: u16 = 0x1fff;
const RAM_BUS_PINS: u16 = 0b0000_0111_1111_1111;
const PPU_REGISTERS: u16 = 0x2000;
const PPU_REGISTERS_MIRRORS_END: u16 = 0x3fff;
const PPU_BUS_PINS: u16 = 0b0010_0000_0000_0111;
const ROM: u16 = 0x8000;
const ROM_END: u16 = 0xffff;

/// - 0x0000 .. 0x1fff : CPU RAM
/// - 0x2000 .. 0x3fff : PPU Registers
/// - 0x8000 .. 0xffff : PRG ROM

impl Bus {
    pub fn new(rom: Rom) -> Self {
        Bus {
            cpu_vram: [0; 2048],
            rom,
        }
    }
    fn read_prg_rom(&self, mut addr: u16) -> u8 {
        addr -= 0x8000;
        if self.rom.prg_rom.len() == 0x4000 && addr >= 0x4000 {
            addr = addr & 0x4000;
        }
        self.rom.prg_rom[addr as usize]
    }
}


impl Mem for Bus {
    fn mem_read(&self, addr: u16) -> u8 {
        match addr {
            RAM..=RAM_MIRRORS_END => {
                let mirror_down_addr = addr & RAM_BUS_PINS;
                self.cpu_vram[mirror_down_addr as usize]
            }
            PPU_REGISTERS..=PPU_REGISTERS_MIRRORS_END => {
                let _mirror_down_addr = addr & PPU_BUS_PINS;
                todo!("PPU is not supported yet")
            }
            ROM..=ROM_END => {
                self.read_prg_rom(addr)
            }
            _ => {
                debug!("Ignoring mem access at {}", addr);
                0
            }
        }
    }
    fn mem_write(&mut self, addr: u16, data: u8) {
        match addr {
            RAM..=RAM_MIRRORS_END => {
                let mirror_down_addr = addr & RAM_BUS_PINS;
                self.cpu_vram[mirror_down_addr as usize] = data;
            }
            PPU_REGISTERS..=PPU_REGISTERS_MIRRORS_END => {
                let _mirror_down_addr = addr & RAM_BUS_PINS;
                todo!("PPU is not supported yet")
            }
            ROM..=ROM_END => {
                debug!("Attempt to write to Cartridge ROM space")
            }
            _ => {
                debug!("Ignoring mem write-access at {}", addr);
            }
        }
    }
}