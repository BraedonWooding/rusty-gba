use clap::{Parser, Subcommand, ValueEnum};

mod hex_parser;
use hex_parser::HexParser;

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Option<Command>,
}

#[derive(Subcommand)]
enum Command {
    /// Assemble an assembly .s file into a .gba rom, this only supports
    /// assembly, and not something like a .rs/.c/whatever file.
    Asm {
        #[arg(short, long, value_name = "FILE")]
        /// The file to assemble
        file: Option<String>,

        #[arg(value_name = "INSTRUCTION")]
        /// The raw instruction to assemble, this is useful for testing
        /// specific instructions without needing to create a file.
        instruction: Option<String>,
    },
    /// Disassemble a .gba rom into an assembly .s file, this will not
    /// disassemble the entire ROM, but rather just the code section.
    Disasm {
        #[arg(short, long, value_name = "FILE")]
        /// The file to disassemble
        file: Option<String>,

        #[arg(value_name = "HEX", value_parser = HexParser{})]
        /// The raw value to disassemble, this is useful for testing
        /// specific instructions without needing to create a file.
        hex: Option<u32>,
    },
    /// Handles patching both existing .gba roms, and any .o/.bin files
    /// which contain compiled ARMv7TDMI code that hasn't been compiled for the GBA.
    ///
    /// For example, this will patch / include the header.  We try to detect
    /// quite a few edge cases, but you might need to specify the specific cases
    /// for some ROMs.
    Patch {},
    /// For running the GBA emulator, this will run the GBA emulator
    /// with the specified ROM, and will also handle the BIOS.
    /// You can load a rom after the fact, or you can specify a ROM to load
    /// when running the emulator.
    Run {
        #[arg(short, long, value_name = "FILE")]
        /// The file to run, this will be loaded into the emulator.
        file: Option<String>,

        #[arg(short, long, value_name = "BIOS")]
        /// The BIOS file to use.  If not specified, we will use
        /// our custom BIOS from the `bios` crate.
        bios: Option<String>,

        #[arg(short, long)]
        /// Whether to automatically patch the ROM, we by default patch.
        no_auto_patch: bool,

        #[arg(value_enum, short, long, default_value_t, value_name = "MODE")]
        /// The mode to run the emulator in, for example headless is used in CI/CD.
        mode: Mode,
    },
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, ValueEnum)]
pub enum Mode {
    // Note: mode is only available when the `gui` feature is enabled.
    #[cfg_attr(feature = "gui", default)]
    /// Uses SDL2 to run the emulator with a graphical interface. This requires SDL2 to be installed.
    GUI,
    #[cfg_attr(not(feature = "gui"), default)]
    /// Runs the emulator in a headless mode, this is useful for CI/CD.
    Headless,
}

fn main() -> Result<(), anyhow::Error> {
    let cli = Cli::parse();

    match cli.command {
        Some(Command::Asm { file, instruction }) => {
            if let Some(file) = file {
                println!("Assembling file: {}", file);
                // Call the assembly logic here
                Ok(())
            } else if let Some(instruction) = instruction {
                println!("Assembling instruction: {}", instruction);
                // Call the assembly logic for a single instruction here
                Ok(())
            } else {
                Err(anyhow::anyhow!(
                    "No file or instruction specified for assembly."
                ))
            }
        }
        Some(Command::Disasm { file, hex }) => {
            if let Some(file) = file {
                println!("Disassembling file: {}", file);
                // Call the disassembly logic here
                Ok(())
            } else if let Some(hex) = hex {
                println!("Disassembling hex value: 0x{:X}", hex);
                // Call the disassembly logic for a single hex value here
                Ok(())
            } else {
                Err(anyhow::anyhow!(
                    "No file or hex value specified for disassembly."
                ))
            }
        }
        Some(Command::Patch {}) => {
            println!("Patching ROM...");
            // Call the patching logic here
            Ok(())
        }
        Some(Command::Run {
            file,
            bios,
            no_auto_patch,
            mode,
        }) => {
            if mode == Mode::GUI && !cfg!(feature = "gui") {
                return Err(anyhow::anyhow!(
                    "GUI mode is not available. Please enable the 'gui' feature."
                ));
            }

            if !no_auto_patch {
                println!("Automatically patching ROM...");
                // Call the patching logic here
            }

            println!("Running emulator in {:?} mode", mode);
            Ok(())
        }
        None => Err(anyhow::anyhow!("No command specified.")),
    }
}
