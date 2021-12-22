FL = fl

BIFROST = ./bifrost
BIFROST_EXE = $(BIFROST)/bifrost
BIFROST_EXAMPLES = $(BIFROST)/examples

PROCESSOR = ./RTL/cephalopode.fl
ALU_TEST = ./RTL/ALU/ALU_test.fl
EXAMPLES = ./compile/examples.fl

DOCKER = sudo docker
DOCKER_IMG_NAME = processor
DOCKER_ZIP_NAME = docker_image.zip

BIFROST_EXAMPLES_LIST = \
	$(addprefix blockcipher/, cbc cipher_caesar sink source) \
	$(addprefix fib/, fib myadder)

BIFROST_ALU_LIST = $(addprefix RTL/ALU/arith/, div mul)

color = on

# =============================
# makefile code and variable setting
# =============================

ifeq ($(color),on)
	# Bold orange text
	color_yellow = \033[33;1m
	color_orange = \033[33m
	color_reset  = \033[38;22m
else
	color_yellow=
	color_reset=
endif

BIFROST_EXAMPLE_TARGETS = $(addprefix $(BIFROST_EXAMPLES)/, $(addsuffix .prog.fl,$(BIFROST_EXAMPLES_LIST)))
BIFROST_ALU_TARGETS = $(addsuffix .prog.fl, $(BIFROST_ALU_LIST))

# =============================
# Default target
# =============================

default: processor

# =============================
# General rules
# =============================

$(BIFROST_EXE):
	echo "$(color_yellow)Building $@$(color_reset)"
	$(MAKE) -C $(BIFROST)
	echo "$(color_yellow)Cleaning build files $@$(color_reset)"
	$(MAKE) -C $(BIFROST) clean

%.prog.fl: %.prog $(BIFROST_EXE)
	echo "$(color_yellow)Compiling $@ with bifrost$(color_reset)"
	$(BIFROST_EXE) $<

# =============================
# Special Targets
# =============================

# No display of executed commands.
$(VERBOSE).SILENT:

.PHONY: \
	bifrost bifrost-alu bifrost-clean bifrost-examples \
	processor ALU-test \
	rom-images rom-images-clean \
	clean clean-all \
	docker-build docker-run docker_zip \
	benchmark1 benchmark2 benchmark3 benchmark4 benchmark5

# =============================
# Bifrost
# =============================

bifrost: $(BIFROST_EXE) ## Create the bifrost executable

bifrost-alu: $(BIFROST_ALU_TARGETS) ## Compile the divider and multiplier (in RTL/ALU/arith) to HFL
bifrost-examples: $(BIFROST_EXAMPLE_TARGETS) ## Compile the bifrost examples (in bifrost/examples/) to HFL

bifrost-clean: ## Remove bifrost build files (but not executable)
	echo "$(color_yellow)Cleaning bifrost build files$(color_reset)"
	$(MAKE) -C bifrost clean

bifrost-clean-exe: ## Remove the bifrost executable
	echo "$(color_yellow)Deleting bifrost executable$(color_reset)"
	-rm -f $(BIFROST_EXE)

# =============================
# Benchmaks
# =============================

compile/_benchmark%.rom:
	echo "$(color_yellow)Building example ROM images in ./compile$(color_reset)"
	$(FL) -d -C -f $(EXAMPLES)

rom-images: compile/_benchmark1.rom ## Create ROM images for benchmarks

rom-images-clean: ## Remove ROM images for benchmarks
	echo "$(color_yellow)Cleaning ccmpile build files$(color_reset)"
	-rm -rf compile/_*.rom

benchmark1: compile/_benchmark1.rom ## Run benchmark 1 - Sum 10 numbers using 2 chunks
	$(FL) -f RTL/benchmark.fl _benchmark1.rom 10000

benchmark2: compile/_benchmark2.rom ## Run benchmark 2 - Multiply 10 numbers using 3 chunks
	$(FL) -f RTL/benchmark.fl _benchmark2.rom 10000

benchmark3: compile/_benchmark3.rom ## Run benchmark 3 - Divide 10 numbers
	$(FL) -f RTL/benchmark.fl _benchmark3.rom 10000

benchmark4: compile/_benchmark4.rom ## Run benchmark 4 - Some factorials to compute choose
	$(FL) -f RTL/benchmark.fl _benchmark4.rom 20000

benchmark5: compile/_benchmark5.rom ## Run benchmark 5 - Crossproduct needing 5 chunks
	$(FL) -f RTL/benchmark.fl _benchmark5.rom 2500

# =============================
# Processor and ALU
# =============================

processor: ## Run the processor on an example and display reduction graph
	$(FL) -f $(PROCESSOR)

ALU-test: ## Run the ALU test file
	$(FL) -f $(ALU_TEST)

clean: bifrost-clean compile-clean ## Remove build files

clean-all: clean bifrost-clean-exe ## Remove all generated files

# =============================
# Docker
# =============================

docker-build: bifrost-clean-exe ## Build the docker image
	echo "$(color_yellow)Building docker image$(color_reset)"
	$(DOCKER) build -t $(DOCKER_IMG_NAME) .

docker-run: ## Run the docker image (requires building first)
	echo "$(color_yellow)Running docker image$(color_reset)"
	$(DOCKER) run --rm -it -e DISPLAY=${DISPLAY} -v /tmp/.X11-unix:/tmp/.X11-unix $(DOCKER_IMG_NAME) /bin/bash

docker-zip: ## Zip the docker image for export
	echo "$(color_yellow)Zipping docker image to $(DOCKER_ZIP_NAME)$(color_reset)"
	$(DOCKER) save $(DOCKER_IMG_NAME) | gzip > $(DOCKER_ZIP_NAME)

help: ## Show this help
	echo "$(color_yellow)make:$(color_reset) usefull targets:"
	egrep -h '\s##\s' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "  $(color_orange)%-17s$(color_reset) %s\n", $$1, $$2}'
