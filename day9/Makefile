ALL: clean build run

clean: .FORCE
	@$(RM) -rf $(PWD)/_build
	@$(RM) -f $(PWD)/day9.native
	@$(RM) -f $(PWD)/day9.byte

adventStdLib.cma: clean 
	@ocamlbuild -quiet -use-ocamlfind -cflags "-w +a-4-6-7-8-9-26-27-29..39-42..42-44-45-48-50-60" adventStdLib.cma

# Friendly alias
stdlib: adventStdLib.cma

day9.byte: stdlib 
	@ocamlbuild -quiet -use-ocamlfind day9.byte

build: day9.byte
	
run: build
	@OCAMLRUNPARAM=b ./day9.byte

.FORCE:

