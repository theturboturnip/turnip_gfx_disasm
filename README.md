# turnip_gfx_disasm

This crate disassembles various flavors (one currently, but shush) of compiled GPU bytecode to an abstract representation. Potential use cases include
- Dependency tracking (what inputs affect specific output values)
- Shader decompilation (similar to my Python application [dx-disassembler](https://github.com/theturboturnip/dx-disassembler)).

It is built to be used in the yk_gfx_disasm crate, alongside [amd_dx_gsa](https://github.com/theturboturnip/amd_dx_gsa),
for decompiling and understanding the shaders used in the Ryu Ga Gotoku (Like A Dragon, or Yakuza) game franchise.
This being the case, the current scope is limited: only AMD RDNA2 bytecode will be supported, and the only available analysis will be dependency tracking.

## Current status

- Could theoretically parse a very small subset of AMD RDNA2 bytecode

## Future work

- Add tests/examples of parsing real shaders
- Fill out the RDNA2 backend
- Dependency tracking
- Experiment with complex shaders
