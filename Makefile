
OCAMLC = ocamlc -g
OCAMLLEX = ocamllex
OCAMLYACC = ocamlyacc -v
OCAMLMKTOP = ocamlmktop
MENHIR = menhir
OCAMLDEP = ocamldep
OCAMLDSORT = ocamldsort
INSTALL = install
PREFIX = /usr/local

COBJ = 

# Source plus generated files.
OCAMLSRC := m6502.ml parser.ml lexer.ml insn.ml pasta.ml expr.ml encode.ml \
	    parse_file.ml layout.ml env.ml context.ml var.ml graph.ml alloc.ml \
	    temps.ml colour.ml line.ml log.ml synthbranch.ml

OCAMLOBJ := $(shell < .depend $(OCAMLDSORT) -byte $(OCAMLSRC))

TARGET = pasta

all:	$(TARGET)

clean:
	rm -f *.cmo *.cmi $(TARGET) parser.ml lexer.ml

cleaner: clean
	rm -f .depend

.PHONY:	test
test:	all
	ocamlfind ocamlc -package kaputt,str -linkpkg runtests.ml -o runtests
	./runtests

install: all
	$(INSTALL) $(TARGET) $(PREFIX)/bin

ML_ERROR:
	@echo Some sort of Ocaml dependency error occurred.
	@false

# core compiler
$(TARGET): $(OCAMLOBJ)
	$(OCAMLC) $(OCAMLOBJ) -o $@

cmmtest: $(CMMTOBJ)
	$(OCAMLMKTOP) $(CMMTOBJ) -o $@

cmmparse: $(CMMPOBJ)
	$(OCAMLC) $(CMMPOBJ) -o $@

# Also include (lex, yacc) generated files here.
.depend:	$(OCAMLSRC)
	$(OCAMLDEP) $(OCAMLSRC) > .depend

%.cmo: %.ml
	$(OCAMLC) $< -c -o $@

%.cmi: %.mli
	$(OCAMLC) $< -c -o $@

%.ml: %.mly
	$(MENHIR) --infer $<

%.ml: %.mll
	$(OCAMLLEX) $<

# Extra dependencies.
parser.ml:	insn.cmo line.cmo
insn.cmo:	expr.cmo m6502.cmo context.cmo line.cmo
lexer.cmo:	parser.cmo
expr.cmo:	env.cmo
context.cmo:	var.cmo log.cmo

ifneq ($(MAKECMDGOALS),clean)
ifneq ($(MAKECMDGOALS),cleaner)
include .depend
endif
endif
