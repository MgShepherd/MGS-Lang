CC := clang
SRC_DIR := src
BUILD_DIR := build

LLVM_C_FLAGS := $(shell llvm-config --cflags)
LLVM_LD_FLAGS := $(shell llvm-config --ldflags)
LLVM_LIBS := $(shell llvm-config --libs core)
LLVM_SYSLIBS := $(shell llvm-config --system-libs)
CFLAGS := -Wall -Wextra -g -std=c23 -I./include $(LLVM_C_FLAGS)

LIBS := $(LLVM_LIBS) $(LLVM_SYSLIBS)
LD_FLAGS := $(LLVM_LD_FLAGS)

TARGET := $(BUILD_DIR)/Compiler

rwildcard = $(foreach d,$(wildcard $1/*),$(call rwildcard,$d,$2) $(filter $(subst *,%,$2),$d))
SRCS := $(call rwildcard,$(SRC_DIR),*.c)
OBJS := $(SRCS:$(SRC_DIR)/%.c=$(BUILD_DIR)/%.o)

.PHONY: all clean

all: $(TARGET)

$(TARGET): $(OBJS)
	$(CC) $(LD_FLAGS) $(OBJS) -o $@ $(LIBS)

$(BUILD_DIR)/%.o: $(SRC_DIR)/%.c
	@mkdir -p $(@D)
	$(CC) $(CFLAGS) -c $< -o $@

$(BUILD_DIR)/%.o: $(TEST_DIR)/%.c
	$(CC) $(CFLAGS) -c $< -o $@

run: $(TARGET)
	$(TARGET)

test: $(TARGET)
	go run ./tests

clean:
	rm -rf $(BUILD_DIR)
