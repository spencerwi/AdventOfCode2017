ALL: clean build run

clean: .FORCE
	@$(RM) -rf $(PWD)/_build
	@$(RM) -f $(PWD)/day3.native

adventStdLib.cma: clean 
	@ocamlbuild -quiet -use-ocamlfind -cflags "-w +a-4-6-7-8-9-36-37-39..39-43..43-44-45-48-50-60" adventStdLib.cma

# Friendly alias
stdlib: adventStdLib.cma

day3.native: stdlib
	@ocamlbuild -quiet -use-ocamlfind day3.native

build: day3.native
	
run: build
	@./day3.native

.FORCE:
