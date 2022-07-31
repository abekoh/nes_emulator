use std::fs::metadata;

use bitflags::bitflags;

use crate::rom::Mirroring;

pub struct PPU {
    chr_rom: Vec<u8>,
    palette_table: [u8; 32],
    vram: [u8; 2048],
    oam_data: [u8; 256],
    mirroring: Mirroring,
    addr: AddrRegister,
    ctrl: ControlRegister,
    internal_data_buf: u8,
}

const CHR_ROM_BEGIN: u16 = 0x0000;
const CHR_ROM_END: u16 = 0x1fff;
const VRAM_BEGIN: u16 = 0x2000;
const VRAM_END: u16 = 0x2fff;
const UNUSED_BEGIN: u16 = 0x3000;
const UNUSED_END: u16 = 0x3eff;
const PALETTE_BEGIN: u16 = 0x3f00;
const PALETTE_END: u16 = 0x3fff;

impl PPU {
    pub fn new(chr_rom: Vec<u8>, mirroring: Mirroring) -> Self {
        PPU {
            chr_rom,
            mirroring,
            vram: [0; 2048],
            oam_data: [0; 64 * 4],
            palette_table: [0; 32],
            addr: AddrRegister::new(),
            ctrl: ControlRegister::new(),
            internal_data_buf: 0,
        }
    }
    pub fn write_to_ppu_addr(&mut self, value: u8) {
        self.addr.update(value);
    }
    fn write_to_ctrl(&mut self, value: u8) {
        self.ctrl.update(value);
    }
    fn increment_vram_addr(&mut self) {
        self.addr.increment(self.ctrl.vram_addr_increment());
    }
    pub fn read_data(&mut self) -> u8 {
        let addr = self.addr.get();
        self.increment_vram_addr();

        match addr {
            CHR_ROM_BEGIN..=CHR_ROM_END => {
                let result = self.internal_data_buf;
                self.internal_data_buf = self.chr_rom[addr as usize];
                result
            }
            VRAM_BEGIN..=VRAM_END => {
                let result = self.internal_data_buf;
                self.internal_data_buf = self.vram[self.mirror_vram_addr(addr) as usize];
                result
            }
            UNUSED_BEGIN..=UNUSED_END => panic!("addr space 0x3000..0x3eff is not expected to be used, requested = {}", addr),
            PALETTE_BEGIN..=PALETTE_END => {
                // The palette data is placed immediately on the data bus, and hence no dummy read is required.
                self.palette_table[(addr - 0x3f00) as usize]
            }
            _ => panic!("unexpected access to mirrored space {}", addr),
        }
    }

    // VRAM:
    //   A : 0x0000 .. 0x0400
    //   B : 0x0400 .. 0x0800
    // PPU Memory Map (Horizontal):
    //   [ a1 : 0x2000 ] [ a2 : 0x2400 ]
    //   [ b1 : 0x2800 ] [ b2 : 0x2c00 ]
    // PPU Memory Map (Vertical):
    //   [ a1 : 0x2000 ] [ b1 : 0x2400 ]
    //   [ a2 : 0x2800 ] [ b2 : 0x2c00 ]
    fn mirror_vram_addr(&self, addr: u16) -> u16 {
        const SPACE_SIZE: u16 = 0x0400;
        let mirrored_vram = addr & 0b0010_1111_1111_1111;
        let vram_index = mirrored_vram - VRAM_BEGIN;
        let name_table = vram_index / SPACE_SIZE;
        match (&self.mirroring, name_table) {
            (Mirroring::HORIZONTAL, 0) => vram_index,                  // a1 -> A
            (Mirroring::HORIZONTAL, 1) => vram_index - SPACE_SIZE,     // a2 -> A
            (Mirroring::HORIZONTAL, 2) => vram_index - SPACE_SIZE,     // b1 -> B
            (Mirroring::HORIZONTAL, 3) => vram_index - SPACE_SIZE * 2, // b2 -> B
            (Mirroring::VERTICAL, 0) => vram_index,                    // a1 -> A
            (Mirroring::VERTICAL, 1) => vram_index,                    // b1 -> B
            (Mirroring::VERTICAL, 2) => vram_index - SPACE_SIZE * 2,   // a2 -> A
            (Mirroring::VERTICAL, 3) => vram_index - SPACE_SIZE * 2,   // b2 -> B
            _ => unreachable!()
        }
    }
}

pub struct AddrRegister {
    hi: u8,
    lo: u8,
    hi_ptr: bool,
}

impl AddrRegister {
    pub fn new() -> Self {
        Self {
            hi: 0,
            lo: 0,
            hi_ptr: true,
        }
    }

    fn set(&mut self, data: u16) {
        self.hi = (data >> 8) as u8;
        self.lo = (data & 0xff) as u8;
    }

    fn get(&self) -> u16 {
        ((self.hi as u16) << 8) | (self.lo as u16)
    }

    fn update(&mut self, data: u8) {
        if self.hi_ptr {
            self.hi = data;
        } else {
            self.lo = data;
        }
        self.mirror_down();
        self.hi_ptr = !self.hi_ptr;
    }

    fn increment(&mut self, inc: u8) {
        let (new_lo, over) = self.lo.overflowing_add(inc);
        self.lo = new_lo;
        if over {
            self.hi = self.hi.wrapping_add(1);
        }
        self.mirror_down();
    }

    fn mirror_down(&mut self) {
        if self.get() > 0x3fff {
            self.set(self.get() & 0b0011_1111_1111_1111);
        }
    }

    fn reset_latch(&mut self) {
        self.hi_ptr = true;
    }
}

bitflags! {
    pub struct ControlRegister: u8 {
        const NAMETABLE1              = 0b0000_0001;
        const NAMETABLE2              = 0b0000_0010;
        const VRAM_ADD_INCREMENT      = 0b0000_0100;
        const SPRITE_PATTERN_ADDR     = 0b0000_1000;
        const BACKGROUND_PATTERN_ADDR = 0b0001_0000;
        const SPRITE_SIZE             = 0b0010_0000;
        const MASTER_SLAVE_SECLET     = 0b0100_0000;
        const GENERATE_NMI            = 0b1000_0000;
    }

}

impl ControlRegister {
    pub fn new() -> Self {
        ControlRegister::from_bits_truncate(0b0000_0000)
    }

    pub fn vram_addr_increment(&self) -> u8 {
        if !self.contains(ControlRegister::VRAM_ADD_INCREMENT) {
            0b0000_0001
        } else {
            0b0010_0000
        }
    }

    pub fn update(&mut self, data: u8) {
        self.bits = data;
    }
}