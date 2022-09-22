use std::path::PathBuf;

use clap::Parser;
use turnip_gfx_disasm::abstract_machine::analysis::dependency::ScalarDependencies;
use turnip_gfx_disasm::rdna2::vm::RDNA2DataRef;
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

    let actions = decoder
        .decode(shader.as_slice())
        .expect("Whoops, error decompiling");

    let mut resolver = ScalarDependencies::new();
    for a in actions {
        resolver.accum_action(a.as_ref());
    }

    for dependent in resolver.dependents() {
        match dependent.0 {
            RDNA2DataRef::Output { .. } => {
                println!("Output {:?} depends on {:?}", dependent.0, dependent.1)
            }
            _ => {}
        }
    }
}
