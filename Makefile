.PHONY: all clean re depend run trim test 

SHELL=/bin/bash
MOSML=${HOME}/mosml
MOSMLTOOLS=${MOSML}/bin/camlrunm ${MOSML}/tools
MOSMLC=${MOSML}/bin/mosmlc -c -liberal
MOSMLL=${MOSML}/bin/mosmlc 
MOSMLYAC=${MOSML}/bin/mosmlyac
MOSMLLEX=${MOSML}/bin/mosmllex

TARGET=dtc

all: $(TARGET)

run: $(TARGET)
	./$(TARGET)

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
topsort.uo: topsort.ui hash.ui 
escape.ui: ast.ui 
temp.uo: temp.ui 
ast.uo: ast.ui 
lexer.uo: parser.ui lineno.uo 
escape.uo: escape.ui common.uo ast.ui hash.ui 
common.uo: ast.ui 
parser.uo: parser.ui ast.ui lineno.uo 
main.uo: parser.ui lexer.uo common.uo ast.ui semantics.ui escape.ui 
hash.uo: hash.ui 
semantics.ui: ast.ui 
frame.uo: frame.ui temp.ui 
parser.ui: ast.ui 
semantics.uo: semantics.ui topsort.ui types.uo common.uo ast.ui hash.ui 
