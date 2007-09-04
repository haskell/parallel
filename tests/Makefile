# This Makefile runs the tests using GHC's testsuite framework.  It
# assumes the package is part of a GHC build tree with the testsuite
# installed in ../../../testsuite.

TOP=../../../testsuite
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk


.PHONY: test068
test068 :
	@echo Compiling
	$(TEST_HC) -v0 -fforce-recomp --make conc068.hs -o conc068 -threaded
	@echo Running
	$(PYTHON) -c 'for i in range(11111): print "abqszzzq"' | ./conc068 +RTS -N2
	@echo Done
