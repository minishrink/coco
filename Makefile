ifndef VERBOSE
.SILENT:
endif

.PHONY: clean example run-example

EXAMPLEDIR=examples
BUILDDIR=_build/default
LIBDIR=automata

example: $(EXAMPLEDIR)/number.ml
	jbuilder build $(EXAMPLEDIR)/number.exe

libs: $(LIBDIR)/fsa.ml
	jbuilder build $(LIBDIR)/automata.a

run-example: $(EXAMPLEDIR)/number.ml
	jbuilder build $(EXAMPLEDIR)/number.exe
	./$(BUILDDIR)/$(EXAMPLEDIR)/number.exe

clean:
	jbuilder clean

reindent:
	ocp-indent -i **/*.ml*
