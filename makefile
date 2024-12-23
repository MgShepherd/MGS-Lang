OUT_DIR = build

compiler: | $(OUT_DIR)
	dune build
	rm -rf _build

all: compiler

$(OUT_DIR):
	mkdir -p $(OUT_DIR)
