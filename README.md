




This repository contains the source code for the IoTProc processor. IoTProc is a custom processor designed for IoT. It aims to be a very low power hardware device directly aimed at running a high-level functional language.

IoTProc is developed as part of the [Octopi Project](https://octopi.chalmers.se/).

This repository also contains the Bifröst compiler, a high-level synthesis tool used to create RTL code for IoTProc from the eponymous language.

**Contents:**
- [Repository description](#repository-description)
- [Running with Docker](#running-with-docker)
- [Makefile targets](#makefile-targets)
- [Manual installation](#manual-installation)
	- [Bifröst](#bifröst)
	- [IoTProc](#iotproc)
- [Building docker](#building-docker)

## Repository description

This repository is organized as follows:
- `bifrost/` - source for the Bifröst compiler, can be built with `make bifrost`
- `bifrost/examples/` - small code examples in Bifröst, can be compiled to FL with `make bifrost-examples`
- `RTL/` - an RTL description of the IoTProc processor written in HFL. A fast example can be run by loading the `cephalopode.fl` main file. (run `make processor`)
- `RTL/ALU/` - the IoTProc ALU, performs operations on arbitrary-precision integers. It contains some parts in state machine editor FsM (`.fsm` files), some in Bifröst (`.prog` files) as well as their compiled FL code (corresponding `.fsm.fl` and `.prog.fl`)

	You can run `make ALU-test` to load the ALU test suite (`RTL/ALU/ALU_test.fl`) then evaluate the relevant test in the interpretor (for instance `TEST_ADD;`). Beware that some of these can be very long.

	You can run `make bifrost-alu` to recompile the ALU divider (`RTL/ALU/arith/div.prog`) and multiplier (`RTL/ALU/arith/mul.prog`) with bifrost. Their compiled versions are aldready available in the `.prog.fl` files.

- `compile/` - tools to create ROM images for IoTProc
- `Makefile` - see `make help` for a list of targets


## Running with Docker

You can download a docker image from the releases. It allows to run bifröst and the processor easily with all dependencies included.

The docker image is called "processor". It is meant to be executed in interactive mode with acces to an X server for the GUI elements. In order to do so:

1. Unzip and load into docker:

	```
	gzip -d docker_image.gz
	sudo docker load -i docker_image
	```

2. To run the image with acces to the X server on a linux system:

	```
	sudo docker run --rm -it -e DISPLAY=:0 \
	     -v /tmp/.X11-unix:/tmp/.X11-unix \
			 -v "$(HOME)/.Xauthority:/home/user/.Xauthority" \
			 processor /bin/bash
	```
	It can be run without the X-server with (but fl will not work then):
	```
	sudo docker run --rm -it processor /bin/bash
	```
	This opens a bash shell on a small debian system. For other systems, the best I can do is point you to [this tutorial](https://cuneyt.aliustaoglu.biz/en/running-gui-applications-in-docker-on-windows-linux-mac-hosts/) which helped me set it up in linux.

	If running on a remote system, [this other tutorial](https://blog.yadutaf.fr/2017/09/10/running-a-graphical-app-in-a-docker-container-on-a-remote-server/) may help set it up

3. You are now logged in as user "user" who has sudo priviledges with password "password". User's home folder contains our souce code under `src`. The makefile in `~/src` can be used to quickly launch all our programs. See `make help` for a full list of targets, or below for specific targets.

	For convenience, we added the fl interpretor and bifrost compiler to the path. They can be accessed with `fl` and `bifrost`. Note that `fl` requires a connection to the X-server.

## Makefile targets

**For bifrost:**
* The examples can be compiled with `make bifrost-examples`. The generated files are the same name `.prog.fl` files.
* The multiplier and divider can be compiled with `make bifrost-alu`. Th
* Bifrost is precompiled, but can be recompiled from the Haskell source with `make bifrost-clean-exe bifrost`.

**For the ALU:** the small tests are fine but arithmetic test can take a >1h and a >16GB of RAM when running on more than (1,1) chunks. To run the ALU tests:
1. Run `make ALU-test`. This will open and fl window and define (but not run) the test
2. To run a test, simply type `TEST_NAME;` in the fl interpretor. Defined tests are
	- TEST_EQ, TEST_NEQ, TEST_GT, TEST_GEQ and TEST_COMPARATOR which runs them all. These take less than a minute each.
	- TEST_NOT, TEST_AND, TEST_OR, TEST_COND and TEST_LOGICAL which runs them all. These take less than a minute each.
	- TEST_ADD, TEST_SUB, TEST_MUL, TEST_DIV and TEST_ARITHMETIC which runs them all. Running TEST_ARITHMETIC can take around 30 minutes.
	- TEST_ALU: runs all of the above, takes less then 30 min and 16 GB of RAM
	- TEST_ADD_LONG, TEST_SUB_LONG, TEST_MUL_LONG, TEST_DIV_LONG and TEST_ARITHMETIC_LONG. Way more hardware demanding. They take upward of 8 hours and sometime fail due to lack of RAM on my 16 GB pc.
	- Explicit tests TEST_ADD_EX, TEST_SUB_EX, TEST_MUL_EX and TEST_DIV_EX, they can be run with

		```
		TEST_MUL_EX 128 (-1992);
		```

		They are much faster than the symbolic evaluation ones. They print the chunk representation of the operands before running.

**For benchmarks:**
- run `make benchmark1` to run the first benchmark (Adding 10 numbers using 2-chunks). It ends at Time: 20000.
- run `make benchmark2` to run the second benchmark (Multiplying 10 numbers using 3-chunks). It ends at Time: 20000.
- run `make benchmark3` to run the third benchmark (Dividing 10 numbers using 2-chunks). It ends at Time: 20000.
- run `make benchmark4` to run the fourth benchmark (Collection of factorials needing 3 chunks). It ends at Time: 40000.
- run `make benchmark5` to run the fifth benchmark (Crossproduct of 4 pairs using 5 chunks). It ends at Time: 5000.


## Manual installation

### Bifröst

The Bifröst compiler is written in Haskell. It requires the Haskell platform

	sudo apt install ghc bnfc

It can then be built using `make bifrost`. The examples programs in `./bifrost/examples` can be compiled with `make bifrost-examples`.

You can recompile the ALU multiplier and divider with `make bifrost-alu`.

### IoTProc

Building and running IoTProc requires [Voss II](https://github.com/TeamVoss/VossII). To install it:

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
	make -C src install_all_but_yosys
	```

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

## Building docker

You can build the docker image given in the release page from this source code:
- run `make docker-build` to create the image (can take a while)
- run `make docker-run` to run it
- run `make docker-zip` to export it and compress it with gzip
