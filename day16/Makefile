ALL: clean build run

clean: .FORCE
	@$(RM) -rf $(PWD)/_build
	@$(RM) -f $(PWD)/day*.native
	@$(RM) -f $(PWD)/day*.byte

adventStdLib.cma: clean 
	@ocamlbuild -quiet -use-ocamlfind -cflags "-w +a-4-6-7-8-9-26-27-29..39-42..42-44-45-48-50-60" adventStdLib.cma

# Friendly alias
stdlib: adventStdLib.cma

deps: 
	@opam install mparser

day16.native: stdlib deps
	@ocamlbuild -quiet -use-ocamlfind day16.native

build: day16.native
	
run: build
	@OCAMLRUNPARAM=b ./day16.native

.FORCE:

