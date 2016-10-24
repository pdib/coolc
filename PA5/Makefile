
ASSN = 5
CLASS= cs143
CLASSDIR= ../.
LIB= -lfl
AR= gar
ARCHIVE_NEW= -cr
RANLIB= gar -qs

SRC= cgen.cc cgen.h cgen_supp.cc cool-tree.h cool-tree.handcode.h emit.h example.cl README
CSRC= cgen-phase.cc utilities.cc stringtab.cc dumptype.cc tree.cc cool-tree.cc ast-lex.cc ast-parse.cc handle_flags.cc 
TSRC= mycoolc
CGEN=
HGEN= 
LIBS= lexer parser semant
CFIL= cgen.cc cgen_supp.cc ${CSRC} ${CGEN}
LSRC= Makefile
OBJS= ${CFIL:.cc=.o}
OUTPUT= good.output bad.output


CPPINCLUDE= -I. -I${CLASSDIR}/include/PA${ASSN} -I${CLASSDIR}/src/PA${ASSN}


FFLAGS = -d8 -ocool-lex.cc
BFLAGS = -d -v -y -b cool --debug -p cool_yy

CC=g++
CFLAGS=-g -Wall -Wno-unused -Wno-write-strings -Wno-deprecated ${CPPINCLUDE} -DDEBUG
FLEX=flex ${FFLAGS}
BISON= bison ${BFLAGS}
DEPEND = ${CC} -MM ${CPPINCLUDE}

all: cgen

source: ${SRC} ${TSRC} ${LIBS} lsource

lsource: ${LSRC}

#
# if make turnin says "turnin is up to date.", and you want to turnin
# anyway, you can delete the file 'turnin', and do make turnin again.
#
turnin : ${SRC} ${OUTPUT} turnin-clean
	cd ..; ${CLASSDIR}/bin/turnin ${CLASS} PA${ASSN}

turnin-clean:
	@rm -f cgen ${OBJS} *.a *.o *~ *.d

${OUTPUT}:	cgen
	@rm -f ${OUTPUT}
	./mycoolc  example.cl >example.output 2>&1 

compile:	cgen change-prot

change-prot:
	@-chmod 660 ${SRC} ${OUTPUT}

cgen:	${OBJS} ${LIBS}
	${CC} ${CFLAGS} ${OBJS} ${LIB} -o cgen

.cc.o:
	${CC} ${CFLAGS} -c $<

dotest:	cgen example.cl
	@echo "\nRunning code generator on example.cl\n"
	-./mycoolc example.cl

lexer:
	cp ${CLASSDIR}/PA2/lexer lexer

parser:
	cp ${CLASSDIR}/PA3/parser parser

semant:
	cp ${CLASSDIR}/PA4/semant semant

clean :
	-rm -f ${OUTPUT} *.s core ${OBJS} cgen parser semant lexer *~ *.a *.o

clean-compile:
	@-rm -f core ${OBJS} ${LSRC}

%.d: %.cc ${SRC}
	${SHELL} -ec '${DEPEND} $< | sed '\''s/\($*\.o\)[ :]*/\1 $@ : /g'\'' > $@'

-include ${CFIL:.cc=.d}


