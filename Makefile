INFO: 
	@echo "To run an example, run MAKE dayX, where X is a number between 1 and 25"

day%: .FORCE
	@$(MAKE) -C $@

.FORCE:
