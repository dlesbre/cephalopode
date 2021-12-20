# Cephalopode

This repository contains the source code for the Cephalopode processor. Cephalopode is a custom processor designed for IoT. It aims to be a very low power hardware device directly aimed at running a high-level functional language.

Cephalopode is developed as part of the [Octopi Project](https://octopi.chalmers.se/).

This repository also contains the Bifröst compiler, a high-level synthesis tool used to create RTL code for Cephalopode from the eponymous language.

**Contents:**
- [Repository description](#repository-description)
- [Dependencies and installation](#dependencies-and-installation)
	- [Bifröst](#bifröst)
	- [Cephalopode](#cephalopode)
- [References](#references)

## Repository description

This repository is organized as follows:
- `bifrost/` - source for the Bifröst compiler, can be built with `make bifrost`
- `bifrost/examples/` - small code examples in Bifröst, can be compiled to FL with `make bifrost-examples`
- `RTL/` - an RTL description of the Cephalopode processor written in HFL. A fast example can be run by loading the `cephalopode.fl` main file. (run `make cephalopode`)
- `RTL/ALU/` - the Cephalopode ALU, performs operations on arbitrary-precision integers. It contains some parts in Stately (`.fsm` files), some in Bifröst (`.prog` files) as well as their compiled FL code (corresponding `.fsm.fl` and `.prog.fl`)

	You can run `make ALU-test` to load the ALU test suite (`RTL/ALU/ALU_test.fl`) then evaluate the relevant test in the interpretor (for instance `TEST_ADD;`). Beware that some of these can be very long.

	You can run `make bifrost-alu` to recompile the ALU divider (`RTL/ALU/arith/div.prog`) and multiplier (`RTL/ALU/arith/mul.prog`) with bifrost. Their compiled versions are aldready available in the `.prog.fl` files.

- `compile/` - tools to create ROM images for Cephalopode
- `Makefile` - see `make help` for a list of targets

## Dependencies and installation

### Bifröst

The Bifröst compiler is written in Haskell. It requires the Haskell platform

	sudo apt install ghc bnfc

It can then be built using `make bifrost`. The examples programs in `./bifrost/examples` can be compiled with `make bifrost-examples`.

You can recompile the ALU multiplier and divider with `make bifrost-alu`.

### Cephalopode

Building and running Cephalopode requires [Voss II](https://github.com/TeamVoss/VossII). To install it:

1. Install Voss II's dependencies:

	```
	sudo apt install tk gcc g++ doxygen flex bison gawk \
	                 libz-dev tcl-dev tk-dev libc6-dev \
	                 clang libreadline-dev python3 imagemagick pandoc
	```

2. Clone Voss II's repository with SSH or HTTPS (run only ONE of these!)

	```
	git clone git@github.com:TeamVoss/VossII.git
	git clone https://github.com/TeamVoss/VossII.git
	```

3. Run the makefile to build it:

	```
	cd Voss II
	make -C src install
	```

	The makefile may fail when building yosys. This doesn't really matter as we don't use yosys in cephalopode.

4. You can check your install by running the fl interpretor (`.../VossII/bin/fl`). It should open a new window with the fl interpretor.

	Type `load "ste.fl";` in the interpretor to verify that you have the HFL library.

5. You're done. For convenience, you can add the fl interpretor (`.../VossII/bin/fl`) to your PATH:

	```
	export PATH="path/to/VossII/bin:$PATH"
	```

	Or replace the first line of the `Makefile` with the relevant path.

An optionnal dependency is [Stately](https://github.com/popje-chalmers/stately), a custom FSM editor. It is used to read the `.fsm` files and display a visualy editable state machine. The exported versions of these files was also provided as well (with extension `.fsm.fl`). It requires java to run. To install it:

1. Clone the repository with SSH or HTTPS

	```
	git clone git@github.com:popje-chalmers/stately.git
	git clone https://github.com/popje-chalmers/stately.git
	```

2. Build and run

	```
	make run
	```

	And use the interface to navigate to the `.fsm` files.


## References

1. Jeremy Pope and Jules Saget and Carl-Johan H. Seger, *Cephalopode: A custom processor aimed at functional language execution for IoT devices* in *18th ACM/IEEE International Conference on Formal Methods and Models for System Design*, MEMCODE 2020, Jaipur, India. [https://doi.org/10.1109/MEMOCODE51338.2020.9315094](https://doi.org/10.1109/MEMOCODE51338.2020.9315094).
2. Jeremy Pope and Jules Saget and Carl-Johan H. Seger, *Stately: An FSM Design Tool* in *18th ACM/IEEE International Conference on Formal Methods and Models for System Design*, MEMCODE 2020, Jaipur, India. [https://doi.org/10.1109/MEMOCODE51338.2020.9315130](https://doi.org/10.1109/MEMOCODE51338.2020.9315130).
