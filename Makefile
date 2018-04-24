ifndef VERBOSE
.SILENT:
endif

.PHONY: clean test example run-example libs reindent

EXAMPLEDIR=examples
BUILDDIR=_build/default
LIBDIR=automata
TESTDIR=test

clean:
	jbuilder clean

test: $(TESTDIR)/test.ml
	jbuilder build test/test.exe
	./$(BUILDDIR)/$(TESTDIR)/test.exe

example: $(EXAMPLEDIR)/number.ml
	jbuilder build $(EXAMPLEDIR)/number.exe

run-example: $(EXAMPLEDIR)/number.ml
	jbuilder build $(EXAMPLEDIR)/number.exe
	./$(BUILDDIR)/$(EXAMPLEDIR)/number.exe

libs: $(LIBDIR)/fsa.ml
	jbuilder build $(LIBDIR)/automata.a

reindent:
	ocp-indent -i **/*.ml*

