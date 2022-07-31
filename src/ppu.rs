use crate::rom::Mirroring;

pub struct PPU {
    chr_rom: Vec<u8>,
    palette_table: [u8; 32],
    vram: [u8; 2048],
    oam_data: [u8; 256],

    mirroring: Mirroring,
}

impl PPU {
    fn new(chr_rom: Vec<u8>, mirroring: Mirroring) -> Self {
        PPU {
            chr_rom,
            mirroring,
            vram: [0; 2048],
            oam_data: [0; 64 * 4],
            palette_table: [0; 32],
        }
    }
}