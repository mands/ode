# Simple makefile
all: install

install: ode2.lang ion.lang ode3.lang llvm.lang
	mkdir -p ~/.local/share/gtksourceview-2.0/language-specs
	cp -f ./*.lang ~/.local/share/gtksourceview-2.0/language-specs/
	mkdir -p ~/.local/share/gtksourceview-3.0/language-specs
	cp -f ./*.lang ~/.local/share/gtksourceview-3.0/language-specs/


.PHONY: install

