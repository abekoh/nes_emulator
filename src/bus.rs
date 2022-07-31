use log::debug;

use crate::Mem;
use crate::ppu::PPU;
use crate::rom::Rom;

pub struct Bus {
    cpu_vram: [u8; 2048],
    prg_rom: Vec<u8>,
    ppu: PPU,
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
        let ppu = PPU::new(rom.chr_rom, rom.screen_mirrorling);
        Bus {
            cpu_vram: [0; 2048],
            prg_rom: rom.prg_rom,
            ppu,
        }
    }
    fn read_prg_rom(&self, mut addr: u16) -> u8 {
        addr -= 0x8000;
        if self.prg_rom.len() == 0x4000 && addr >= 0x4000 {
            addr = addr & 0x4000;
        }
        self.prg_rom[addr as usize]
    }
}


impl Mem for Bus {
    fn mem_read(&mut self, addr: u16) -> u8 {
        match addr {
            RAM..=RAM_MIRRORS_END => {
                let mirror_down_addr = addr & RAM_BUS_PINS;
                self.cpu_vram[mirror_down_addr as usize]
            }
            0x2000 | 0x2001 | 0x2003 | 0x2005 | 0x2006 | 0x4014 => {
                panic!("Attempt to read from write-only PPU address {:x}", addr)
            }
            0x2007 => self.ppu.read_data(),
            0x2008..=PPU_REGISTERS_MIRRORS_END => {
                let mirror_down_addr = addr & PPU_BUS_PINS;
                self.mem_read(mirror_down_addr)
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
            0x2000 => {
                self.ppu.write_to_ctrl(data);
            }
            0x2006 => {
                self.ppu.write_to_ppu_addr(data);
            }
            0x2007 => {
                self.ppu.write_to_data(data);
            }
            0x2008..=PPU_REGISTERS_MIRRORS_END => {
                let mirror_down_addr = addr & RAM_BUS_PINS;
                self.mem_write(mirror_down_addr, data);
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