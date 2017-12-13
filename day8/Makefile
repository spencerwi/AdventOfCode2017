ALL: clean build run

clean: .FORCE
	@$(RM) -rf $(PWD)/_build
	@$(RM) -f $(PWD)/day8.native
	@$(RM) -f $(PWD)/day8.byte

adventStdLib.cma: clean 
	@ocamlbuild -quiet -use-ocamlfind -cflags "-w +a-4-6-7-8-9-26-27-29..39-42..42-44-45-48-50-60" adventStdLib.cma

# Friendly alias
stdlib: adventStdLib.cma

deps:
	@opam install mparser

day8.native: stdlib deps
	@ocamlbuild -quiet -use-ocamlfind day8.byte

build: day8.native
	
run: build
	@OCAMLRUNPARAM=b ./day8.byte

.FORCE:
