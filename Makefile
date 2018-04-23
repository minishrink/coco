ifndef VERBOSE
.SILENT:
endif

.PHONY: clean example run-example

EXAMPLEDIR=examples
BUILDDIR=_build/default

example: $(EXAMPLEDIR)/number.ml
	jbuilder build $(EXAMPLEDIR)/number.exe

run-example: $(EXAMPLEDIR)/number.ml
	jbuilder build $(EXAMPLEDIR)/number.exe
	./$(BUILDDIR)/$(EXAMPLEDIR)/number.exe

clean:
	jbuilder clean

reindent:
	ocp-indent -i **/*.ml*
