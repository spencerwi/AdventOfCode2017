ALL: clean build run

clean: .FORCE
	@$(RM) -rf $(PWD)/_build
	@$(RM) -f $(PWD)/day1.native

day1.native:
	@ocamlbuild -quiet day1.native

build: day1.native
	
run: build
	@./day1.native

.FORCE:
