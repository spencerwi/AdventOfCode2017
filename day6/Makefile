ALL: clean build run

clean: .FORCE
	@$(RM) -rf $(PWD)/_build
	@$(RM) -f $(PWD)/day6.native

adventStdLib.cma: clean 
	@ocamlbuild -quiet -use-ocamlfind -cflags "-w +a-4-6-7-8-9-26-27-29..39-42..42-44-45-48-50-60" adventStdLib.cma

# Friendly alias
stdlib: adventStdLib.cma

day6.native: stdlib
	@ocamlbuild -quiet -use-ocamlfind day6.native

build: day6.native
	
run: build
	@./day6.native

.FORCE:
