.PHONY: all clean re depend run trim test testgood testbad

SHELL=/bin/bash
MOSML=${HOME}/mosml
MOSMLTOOLS=${MOSML}/bin/camlrunm ${MOSML}/tools
MOSMLC=${MOSML}/bin/mosmlc -c -liberal
MOSMLL=${MOSML}/bin/mosmlc 
MOSMLYAC=${MOSML}/bin/mosmlyac
MOSMLLEX=${MOSML}/bin/mosmllex

all: tiger

run: tiger
	./tiger

PARSER=parser.sml
LEXER=lexer.sml

MODULES=$(patsubst %.sml,%,$(wildcard *.sml))
SRCS=   $(patsubst %,%.sml,${MODULES}) parser.y lexer.lex
OBJS=   $(patsubst %,%.uo,${MODULES}) parser.uo parser.ui lexer.uo lexer.ui

tiger: ${OBJS}
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
	rm -f tiger

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

test: testgood testbad

testgood: tiger
	for i in tests/good/*.tig; do \
		if ./tiger "$$i" &>/dev/null ; then \
			echo "Test $$i ok." ; \
		else \
			echo "Test $$i FALLADO!" ; \
			exit 1 ; \
		fi \
	done

testbad: tiger
	for i in tests/bad/*.tig; do \
		if ./tiger "$$i" &>/dev/null; then \
			echo "Test $$i FALLADO!" ; \
			exit 1 ; \
		else \
			echo "Test $$i ok." ; \
		fi \
	done



# Dependencias autogeneradas:
#
### DO NOT DELETE THIS LINE
lexer.uo: parser.ui lineno.uo 
hash.uo: hash.ui 
escape.ui: ast.ui 
semantics.ui: ast.ui 
topsort.uo: topsort.ui hash.ui 
common.uo: ast.ui 
parser.uo: parser.ui ast.ui lineno.uo 
semantics.uo: semantics.ui topsort.ui types.uo common.uo ast.ui hash.ui 
main.uo: parser.ui lexer.uo common.uo ast.ui semantics.ui escape.ui 
escape.uo: escape.ui common.uo ast.ui hash.ui 
ast.uo: ast.ui 
parser.ui: ast.ui 
