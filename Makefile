FL = fl

BIFROST = ./bifrost
BIFROST_EXE = $(BIFROST)/bifrost
BIFROST_EXAMPLES = $(BIFROST)/examples

CEPHALOPODE = ./RTL/cephalopode.fl
ALU_TEST = ./RTL/ALU/ALU_test.fl
EXAMPLES = ./compile/examples.fl

DOCKER = sudo docker
DOCKER_IMG_NAME = cephalopode
DOCKER_ZIP_NAME = cephalopode.zip

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

default: cephalopode

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
	cephalopode ALU-test \
	compile compile-clean \
	clean clean-all \
	docker-build docker-run

bifrost: $(BIFROST_EXE) ## Create the bifrost executable

bifrost-alu: $(BIFROST_ALU_TARGETS) ## Compile the divider and multiplier to HFL with bifrost
bifrost-examples: $(BIFROST_EXAMPLE_TARGETS) ## Compile the bifrost examples to HFL

bifrost-clean: ## Remove bifrost build files
	echo "$(color_yellow)Cleaning bifrost build files$(color_reset)"
	$(MAKE) -C bifrost clean

compile: ## Compiles example ROM images for cephalopode
	echo "$(color_yellow)Building example ROM images in ./compile$(color_reset)"
	$(FL) -noX -d -C -f $(EXAMPLES)

compile-clean: ## Remove cephalopode build files
	echo "$(color_yellow)Cleaning ccmpile build files$(color_reset)"
	-rm -rf compile/_*.rom

cephalopode: ## Run cephalopode and display reduction graph
	$(FL) -f $(CEPHALOPODE)

ALU-test: ## Run the ALU test file
	$(FL) -f $(ALU_TEST)

clean: bifrost-clean compile-clean ## Remove build files

clean-all: clean ## Remove all generated files
	echo "$(color_yellow)Cleaning bifrost executable$(color_reset)"
	-rm $(BIFROST_EXE)

docker-build: ## Build the docker image
	echo "$(color_yellow)Building docker image$(color_reset)"
	$(DOCKER) build -t $(DOCKER_IMG_NAME) .

docker-run: ## Run the docker image (requires building first)
	echo "$(color_yellow)Running docker image$(color_reset)"
	$(DOCKER) run -it $(DOCKER_IMG_NAME) /bin/bash --rm

docker-zip: ## Zip the docker image for export
	echo "$(color_yellow)Zipping docker image$(color_reset)"
	$(DOCKER) save -o $(DOCKER_ZIP_NAME) $(DOCKER_IMG_NAME)

help: ## Show this help
	echo "$(color_yellow)make:$(color_reset) usefull targets:"
	egrep -h '\s##\s' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "  $(color_orange)%-17s$(color_reset) %s\n", $$1, $$2}'
