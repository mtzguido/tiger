.PHONY: all clean re depend run trim

HOME=/home/guido
MOSML=${HOME}/mosml
MOSMLTOOLS=camlrunm ${MOSML}/tools
MOSMLC=${MOSML}/bin/mosmlc -c -liberal
MOSMLL=${MOSML}/bin/mosmlc 

all: tiger

run: tiger
	./tiger

PARSER=parser.sml
LEXER=lexer.sml

MODULES=parser.sml lexer.sml lineno.sml tiger.sml ast.sml
OBJS= $(patsubst %.sml,%.uo,${MODULES})

tiger: ${OBJS}
	${MOSMLL} tiger.uo -o $@

%.sml: %.lex
	mosmllex $<

%.sml %.sig: %.y
	mosmlyac -v $<

%.ui: %.sig
	${MOSMLC} $<

%.uo: %.sml
	${MOSMLC} $<

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

depend: $(MODULES)
	rm -f Makefile.bak
	mv Makefile Makefile.bak
	$(MOSMLTOOLS)/cutdeps < Makefile.bak > Makefile
	$(MOSMLTOOLS)/mosmldep >> Makefile

### DO NOT DELETE THIS LINE
parser.uo: parser.ui ast.uo lineno.uo 
parser.ui: ast.uo 
lexer.uo: parser.ui lineno.uo 
tiger.uo: parser.ui lexer.uo ast.uo 