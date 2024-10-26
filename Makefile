CC=clang
INC_DIRS=./include/
CFLAGS=-Wall -Wextra -g $(foreach D,$(INC_DIRS),-I$(D)) $(DEP_FLAGS) -MP -MD

BUILD_FOLDER=build/
CODE_DIR=src/
BINARY=$(BUILD_FOLDER)MGS

C_FILES=$(foreach D,$(CODE_DIR),$(wildcard $(D)*.c))
OBJECTS=$(patsubst $(CODE_DIR)%.c,$(BUILD_FOLDER)%.o,$(C_FILES))
DEP_FILES=$(patsubst $(CODE_DIR)%.c,$(BUILD_FOLDER)%.d,$(C_FILES))

all: $(BINARY)

$(BINARY): $(OBJECTS)
	$(CC) -o $@ $^

$(BUILD_FOLDER)%.o:$(CODE_DIR)%.c | $(BUILD_FOLDER)
	$(CC) $(CFLAGS) -c -o $@ $^

$(BUILD_FOLDER):
	mkdir $(BUILD_FOLDER)

clean:
	rm -rf $(BUILD_FOLDER)
