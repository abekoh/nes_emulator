use crate::rom::Mirroring;

pub struct PPU {
    chr_rom: Vec<u8>,
    palette_table: [u8; 32],
    vram: [u8; 2048],
    oam_data: [u8; 256],

    mirroring: Mirroring,
}