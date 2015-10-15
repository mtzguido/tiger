.PHONY: all clean re depend run trim test

SHELL=/bin/bash
MOSML=mosml
MOSMLTOOLS=camlrunm ${MOSML}/tools
MOSMLC=${MOSML}c -c -liberal
MOSMLL=${MOSML}c
MOSMLYAC=mosmlyac
MOSMLLEX=mosmllex

TARGET=dtc

all: $(TARGET)

run: $(TARGET)
	./$(TARGET) -

PARSER=parser.sml
LEXER=lexer.sml

MODULES=$(patsubst %.sml,%,$(wildcard *.sml))
SRCS=   $(patsubst %,%.sml,${MODULES}) parser.y lexer.lex
OBJS=   $(patsubst %,%.uo,${MODULES}) parser.uo parser.ui lexer.uo lexer.ui

$(TARGET): ${OBJS}
	${MOSMLL} main.uo	-o $@

%.sml: %.lex
	${MOSMLLEX} $<

%.sml %.sig: %.y
	${MOSMLYAC} -v $<

%.ui: %.sig
	${MOSMLC} $< 	-o $@

%.uo %.ui: %.sml
	${MOSMLC} $< 	-o $@

clean: trim
	rm -f $(TARGET)

trim:
	rm -f Makefile.bak
	rm -f parser.output
	rm -f lexer.sml
	rm -f parser.sml
	rm -f parser.sig
	rm -f *.o *.ui *.uo

re: clean all

# hay que agregar estas dependencias, si no
# no aparecen cuando hacemos 'make depend'
# sin haber generado el parser/lexer
depend: $(SRCS) parser.sml parser.sig lexer.sml
	rm -f Makefile.bak
	mv Makefile Makefile.bak
	$(MOSMLTOOLS)/cutdeps < Makefile.bak > Makefile
	$(MOSMLTOOLS)/mosmldep >> Makefile

test: $(TARGET)
	. scripts/run_tests.sh

# Dependencias autogeneradas:
#
### DO NOT DELETE THIS LINE
main.uo: parser.ui lexer.uo common.uo ast.uo semantics.ui escape.ui
semantics.uo: semantics.ui ir.ui translate.ui temp.ui topsort.ui types.uo \
    frame.ui common.uo ast.uo hash.ui runtime.uo
parser.uo: parser.ui ast.uo lineno.uo
parser.ui: ast.uo
translate.uo: translate.ui ir.ui frame.ui common.uo liv.ui color.ui
frame.ui: temp.ui ir.ui asm.ui
frame.uo: frame.ui canon.ui codegen.ui asm.ui flowcalc.ui
hash.uo: hash.ui
translate.ui: ir.ui frame.ui
lexer.uo: parser.ui lineno.uo
semantics.ui: ast.uo
temp.uo: temp.ui
common.uo: ast.uo
escape.uo: escape.ui common.uo ast.uo hash.ui
topsort.uo: topsort.ui hash.ui
escape.ui: ast.uo
runtime.uo: types.uo translate.ui
ir.uo: ir.ui
canon.uo: canon.ui common.uo
canon.ui: ir.ui
codegen.uo: codegen.ui
codegen.ui: asm.ui
frame.uo: asm.ui
graph.uo: graph.ui
flowcalc.uo: flowcalc.ui graph.ui flow.ui
flowcalc.ui: flow.ui asm.ui
flow.ui: graph.ui temp.ui
set.uo: set.ui
liv.ui: set.ui graph.ui flow.ui
color.uo: color.ui graph.ui
