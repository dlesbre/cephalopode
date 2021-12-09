FL = fl
BIFROST = ./bifrost/bifrost
CEPHALOPODE = ./RTL/cephalopode.fl
EXAMPLES = ./compile/examples.fl

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

# =============================
# Default target
# =============================

default: cephalopode

# =============================
# General rules
# =============================

$(BIFROST):
	echo "$(color_yellow)Compiling $@$(color_reset)"
	$(MAKE) -C bifrost
	echo "$(color_yellow)Cleaning build files $@$(color_reset)"
	$(MAKE) -C bifrost clean

# =============================
# Special Targets
# =============================

# No display of executed commands.
$(VERBOSE).SILENT:

.PHONY: \
	bifrost bifrost-clean \
	cephalopode \
	compile compile-clean \
	clean clean-all

bifrost: $(BIFROST) ## Create the bifrost executable

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

clean: bifrost-clean compile-clean ## Remove build files

clean-all: clean ## Remove all generated files
	echo "$(color_yellow)Cleaning bifrost executable$(color_reset)"
	-rm $(BIFROST)

help: ## Show this help
	echo "$(color_yellow)make:$(color_reset) usefull targets:"
	egrep -h '\s##\s' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "  $(color_orange)%-17s$(color_reset) %s\n", $$1, $$2}'
