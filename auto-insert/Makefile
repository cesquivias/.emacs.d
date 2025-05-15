BIN := main # TODO : change

SRC_DIR := src
BUILD_DIR := build
TEST_DIR := test

CFLAGS := -g
ASFLAGS := -g

LIBS_SRC := $(wildcard $(SRC_DIR)/lib/*)
LIBS_BUILD := $(patsubst $(SRC_DIR)/%.s, $(BUILD_DIR)/%.o, $(LIBS_SRC))

all: $(BUILD_DIR)/$(BIN)

test: all
	for t in $(TEST_DIR)/*.sh ; do \
		echo Run test $$t ; \
		PATH=$(BUILD_DIR):$$PATH bash $$t ; \
	done

clean:
	$(RM) -r $(BUILD_DIR)

# Create BUILD_DIR
$(BUILD_DIR) $(BUILD_DIR)/lib:
	mkdir -p $@

# asm -> object file
$(BUILD_DIR)/%.o: $(SRC_DIR)/%.s | $(BUILD_DIR) $(BUILD_DIR)/lib
	as -g $< -o $@

# object file -> shared library
$(BUILD_DIR)/%.so: $(BUILD_DIR)/%.o | $(BUILD_DIR) $(BUILD_DIR)/lib
	$(CC) $(CFLAGS) -shared -o $@ $^

# object file -> bin
$(BUILD_DIR)/%: $(LIBS_BUILD) $(BUILD_DIR)/%.o | $(BUILD_DIR)/lib
	ld $^ -o $@ -static -nostdlib

# C files
$(BUILD_DIR)/%: $(SRC_DIR)/%.c
	$(CC) $(CFLAGS) $< -o $@

# test C file -> bin
$(BUILD_DIR)/%: $(LIBS_BUILD) $(TEST_DIR)/%.c
	$(CC) $(CFLAGS)

.PHONY: all test clean
