ifndef VERBOSE
.SILENT:
endif

.PHONY: clean test libs reindent

EXAMPLEDIR=examples
BUILDDIR=_build/default
LIBDIR=automata
TESTDIR=test

clean:
	jbuilder clean

test: $(TESTDIR)/test.ml
	jbuilder build test/test.exe
	./$(BUILDDIR)/$(TESTDIR)/test.exe

libs: $(LIBDIR)/fsa.ml
	jbuilder build $(LIBDIR)/automata.a

reindent:
	ocp-indent -i **/*.ml*

