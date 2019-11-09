
.PHONY: clean test libs reindent

ifndef VERBOSE
.SILENT:
endif

EXAMPLEDIR=examples
BUILDDIR=_build/default
LIBDIR=automata
TESTDIR=test

clean:
	dune clean

test: $(TESTDIR)/test.ml
	dune build test/test.exe
	./$(BUILDDIR)/$(TESTDIR)/test.exe

libs: $(LIBDIR)/fsa.ml
	dune build $(LIBDIR)/automata.a

reindent:
	ocp-indent -i **/*.ml*

