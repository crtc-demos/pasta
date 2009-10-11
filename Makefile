
OCAMLC = ocamlc -g
OCAMLLEX = ocamllex
OCAMLYACC = ocamlyacc -v
OCAMLMKTOP = ocamlmktop
MENHIR = menhir
OCAMLDEP = ocamldep
OCAMLDSORT = ocamldsort

COBJ = 

# Source plus generated files.
OCAMLSRC := m6502.ml parser.ml lexer.ml insn.ml pasta.ml expr.ml encode.ml \
	    layout.ml env.ml

OCAMLOBJ := $(shell < .depend $(OCAMLDSORT) -byte $(OCAMLSRC))

TARGET = pasta

all:	$(TARGET)

clean:
	rm -f *.cmo *.cmi $(TARGET) parser.ml lexer.ml

cleaner: clean
	rm -f .depend

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
parser.ml:	insn.cmo
insn.cmo:	expr.cmo
insn.cmo:	m6502.cmo
lexer.cmo:	parser.cmo
expr.cmo:	env.cmo

ifneq ($(MAKECMDGOALS),clean)
ifneq ($(MAKECMDGOALS),cleaner)
include .depend
endif
endif
