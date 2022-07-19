use log::debug;
use crate::Mem;

pub struct Bus {
    cpu_vram: [u8; 2048],
}

impl Bus {
    pub fn new() -> Self {
        Bus {
            cpu_vram: [0; 2048]
        }
    }
}

const RAM: u16 = 0x0000;
const RAM_MIRRORS_END: u16 = 0x1fff;
const RAM_BUS_PINS: u16 = 0b0000_0111_1111_1111;
const PPU_REGISTERS: u16 = 0x2000;
const PPU_REGISTERS_MIRRORS_END: u16 = 0x3fff;
const PPU_BUS_PINS: u16 = 0b0010_0000_0000_0111;

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
            _ => {
                debug!("Ignoreing mem access at {}", addr);
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
            _ => {
                debug!("Ignoreing mem write-access at {}", addr);
                0
            }
        }
    }
}