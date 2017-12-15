ALL: clean build run

clean: .FORCE
	@$(RM) -rf $(PWD)/_build
	@$(RM) -f $(PWD)/day7.native
	@$(RM) -f $(PWD)/day7.byte

adventStdLib.cma: clean 
	@ocamlbuild -quiet -use-ocamlfind -cflags "-w +a-4-6-7-8-9-26-27-29..39-42..42-44-45-48-50-60" adventStdLib.cma

# Friendly alias
stdlib: adventStdLib.cma

deps:
	@opam install mparser

day7.native: stdlib deps
	@ocamlbuild -quiet -use-ocamlfind day7.byte

build: day7.native
	
run: build
	@OCAMLRUNPARAM=b ./day7.byte

.FORCE:
