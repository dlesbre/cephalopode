# Cephalopode

This repository contains the source code for the Cephalopode processor. Cephalopode is a custom processor designed for IoT. It aims to be a very low power hardware device directly aimed at running a high-level functional language.

Cephalopode is developed as part of the [Octopi Project](https://octopi.chalmers.se/).

This repository also contains the Bifröst compiler, a high-level synthesis tool used to create RTL code for Cephalopode from the eponymous language.

**Contents:**
- [Repository description](#repository-description)
- [Dependencies](#dependencies)
	- [Bifröst](#bifröst)
	- [Cephalopode](#cephalopode)
- [References](#references)

## Repository description

This repository is organized as follows:
- `bifrost/` - source for the Bifröst compiler, can be built with `make bifrost`
- `bifrost/examples/` - small code examples in Bifröst, can be compiled to FL with `make bifrost-examples`
- `RTL/` - an RTL description of the Cephalopode processor written in HFL. A fast example can be run by loading the `cephalopode.fl` main file. (run `make cephalopode`)
- `RTL/ALU/` - the Cephalopode ALU, performs operations on arbitrary-precision integers. It contains some parts in Stately (`.fsm` files), some in Bifröst (`.prog` files) as well as their compiled FL code (corresponding `.fsm.fl` and `.prog.fl`)

	You can run `make ALU-test` to load the ALU test suite (`RTL/ALU/ALU_test.fl`) then evaluate the relevant test in the interpretor (for instance `TEST_ADD;`). Beware that some of these can be very long

- `compile/` - tools to create ROM images for Cephalopode
- `Makefile` - see `make help` for a list of targets

## Dependencies

### Bifröst

The Bifröst compiler is written in Haskell. It requires:
- The Haskell platform
- bnfc ("The BNF Converter", on Ubuntu: `apt install bnfc`)
### Cephalopode

Building and running Cephalopode requires [Voss II](https://github.com/TeamVoss/VossII), which can be downloaded and installed from the linked repository.

Additionnaly, some parts of the processor were designed using [Stately](https://github.com/popje-chalmers/stately), a custom FSM editor. The `.fsm` files should be read with it. We have also provided the exported versions of these files as well (with extension `.fsm.fl`)
 It can also be downloaded and installed by following the link.

## References

1. Jeremy Pope and Jules Saget and Carl-Johan H. Seger, *Cephalopode: A custom processor aimed at functional language execution for IoT devices* in *18th ACM/IEEE International Conference on Formal Methods and Models for System Design*, MEMCODE 2020, Jaipur, India. [https://doi.org/10.1109/MEMOCODE51338.2020.9315094](https://doi.org/10.1109/MEMOCODE51338.2020.9315094).
2. Jeremy Pope and Jules Saget and Carl-Johan H. Seger, *Stately: An FSM Design Tool* in *18th ACM/IEEE International Conference on Formal Methods and Models for System Design*, MEMCODE 2020, Jaipur, India. [https://doi.org/10.1109/MEMOCODE51338.2020.9315130](https://doi.org/10.1109/MEMOCODE51338.2020.9315130).
