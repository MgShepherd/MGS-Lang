OUT_DIR = build

compiler: | $(OUT_DIR)
	opam exec -- dune build

all: compiler

$(OUT_DIR):
	mkdir -p $(OUT_DIR)
