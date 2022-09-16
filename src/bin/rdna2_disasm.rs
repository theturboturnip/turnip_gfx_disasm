use std::path::PathBuf;

use clap::Parser;
use turnip_gfx_disasm::rdna2::RDNA2Decoder;
use turnip_gfx_disasm::Decoder;

#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args {
    /// Shader path
    #[clap(value_parser, default_value = "assets/example.fxc.rdna2")]
    shader_path: PathBuf,
}

fn main() {
    let args = Args::parse();

    let shader = std::fs::read(args.shader_path).expect("couldn't read file");

    let decoder = RDNA2Decoder::new();

    decoder
        .decode(shader.as_slice())
        .expect("Whoops, error decompiling");
}
