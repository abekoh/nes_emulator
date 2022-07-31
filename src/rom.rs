const NES_TAG: [u8; 4] = [0x4e, 0x45, 0x53, 0x1a];
const PRG_ROM_PAGE_SIZE: usize = 16384;
const CHR_ROM_PAGE_SIZE: usize = 8192;

#[derive(Debug, PartialEq)]
#[allow(non_camel_case_types)]
pub enum Mirroring {
    VERTICAL,
    HORIZONTAL,
    FOUR_SCREEN,
}

#[derive(Debug)]
pub struct Rom {
    pub prg_rom: Vec<u8>,
    pub chr_rom: Vec<u8>,
    pub mapper: u8,
    pub screen_mirrorling: Mirroring,
}

impl Rom {
    pub fn new(raw: &Vec<u8>) -> Result<Rom, String> {
        if &raw[0..4] != NES_TAG {
            return Err(String::from("File is not in iNES file format"));
        }

        let ines_ver = (raw[7] >> 2) & 0b11;
        if ines_ver != 0 {
            return Err(String::from("NES2.0 format is not supported"));
        }

        let mapper = (raw[7] & 0b1111_0000) | (raw[6] >> 4);

        let four_screen = raw[6] & 0b1000 != 0;
        let vertical_mirroring = raw[6] & 0b1 != 0;
        let screen_mirroring = match (four_screen, vertical_mirroring) {
            (true, _) => Mirroring::FOUR_SCREEN,
            (false, true) => Mirroring::VERTICAL,
            (false, false) => Mirroring::HORIZONTAL,
        };

        let prg_rom_size = raw[4] as usize * PRG_ROM_PAGE_SIZE;
        let chr_rom_size = raw[5] as usize * CHR_ROM_PAGE_SIZE;

        let skip_trainer = raw[6] & 0b100 != 0;

        let prg_rom_start = 16 + if skip_trainer { 512 } else { 0 };
        let chr_rom_start = prg_rom_start + prg_rom_size;

        Ok(Rom {
            prg_rom: raw[prg_rom_start..(prg_rom_start + prg_rom_size)].to_vec(),
            chr_rom: raw[chr_rom_start..(chr_rom_start + chr_rom_size)].to_vec(),
            mapper,
            screen_mirrorling: screen_mirroring,
        })
    }
}

pub mod test {
    use std::fs;

    use super::*;

    const NES_TEST_PATH: &str = "sample_rom/nestest.nes";
    const HELLO_WORLD_PATH: &str = "sample_rom/hello_world.nes";

    fn load_row_as_raw(path: &str) -> Vec<u8> {
        let rom_bin = fs::read(path).expect(&format!("failed to load {}", path));
        rom_bin
    }

    #[test]
    fn load_nes_test() {
        let raw_rom = load_row_as_raw(NES_TEST_PATH);
        let rom = Rom::new(&raw_rom).expect("failed to load rom");
        assert_eq!(rom.prg_rom.len(), 0x01 * 16384);
        assert_eq!(rom.chr_rom.len(), 0x01 * 8192);
        assert_eq!(rom.mapper, 0b0000_0000);
        assert_eq!(rom.screen_mirrorling, Mirroring::HORIZONTAL);
    }
}