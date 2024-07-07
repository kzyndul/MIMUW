build_dir = build
exe = interpreter

all:
	mkdir -p $(build_dir)
	ghc -Wall -Wextra --make Main.hs -outputdir $(build_dir) -o interpreter -isrc/ -isrc/generated/ -isrc/Interpreter/ -isrc/TypeChecker/

clean:
	rm -rf $(build_dir) $(exe)

.PHONY: clean