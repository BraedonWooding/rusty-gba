use indicatif::ProgressBar;
use std::io::Write;

pub trait PermutationAcceptor {
    fn disassemble(&self, value: u32) -> String;
    fn assemble(&self, instruction: &str) -> u32;
}

// We validate a few things
// 1. u32 -> disassemble -> string -> assemble -> u32 (i.e. you can roundtrip correctly)
// 2. u32 -> disassemble -> string -> real assembler -> u32 (i.e. you disassemble correctly)

fn main() {
    // This generates a '.bin' file that contains every unique permutation of a u32
    // This is useful for testing disassembly and instruction decoding
    let mut file = std::fs::File::create("permutations.bin").unwrap();
    let mut buffer = 0u32;
    let bar = ProgressBar::new((u32::MAX) as u64);
    while buffer < u32::MAX {
        // Write the current value of buffer to the file
        file.write_all(&buffer.to_le_bytes()).unwrap();
        // Increment buffer to the next permutation
        buffer += 1;
        bar.inc(1);
    }

    file.write_all(&buffer.to_le_bytes()).unwrap();
    bar.finish();
    println!("Generated permutations.bin with all unique u32 values.");
}
