S_FILES = build/basic.s
OBJ_FILES = build/basic.o
OUT_DIR = build

%.s:
	dune exec MGSLang

%.o : %.s | $(OUT_DIR)
	as -g $< -o $@

main: $(OBJ_FILES) 
	ld -o $(OUT_DIR)/main $(OBJ_FILES)

all: main

$(OUT_DIR):
	mkdir -p $(OUT_DIR)
