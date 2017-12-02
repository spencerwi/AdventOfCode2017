ALL: clean build run

clean:
	@$(RM) -r _build
	@$(RM) day1.native

day1.native:
	@ocamlbuild -quiet day1.native

build: day1.native
	
run: build
	@./day1.native
