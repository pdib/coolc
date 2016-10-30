
ASSN = 5
CLASS= cs143
CLASSDIR= ../.
LIB= -lfl
AR= ar
ARCHIVE_NEW= -cr
RANLIB= ar -qs

SRC= cgen.cc cgen.h cgen_supp.cc cool-tree.h cool-tree.handcode.h emit.h example.cl README
CSRC= cgen-phase.cc utilities.cc stringtab.cc dumptype.cc tree.cc cool-tree.cc ast-lex.cc ast-parse.cc handle_flags.cc 
TSRC= mycoolc
LIBS= lexer parser semant
CFIL= cgen.cc cgen_supp.cc ${CSRC} ${CGEN}
LSRC= Makefile
OBJS= ${CFIL:.cc=.o}
OUTPUT= good.output bad.output

CPPINCLUDE= -I. -I${CLASSDIR}/include/PA${ASSN} -I${CLASSDIR}/src/PA${ASSN}

CC=g++
CFLAGS=-g -Wall -Wno-unused -Wno-write-strings -Wno-deprecated ${CPPINCLUDE} -DDEBUG
DEPEND = ${CC} -MM ${CPPINCLUDE}

TESTS_SRC=cgen_tests.cc
TESTS=${TESTS_SRC:.cc=.o}

# Points to the root of Google Test, relative to where this file is.
# Remember to tweak this if you move this file.
GTEST_DIR = ../../googletest/googletest/

# Flags passed to the preprocessor.
# Set Google Test's header directory as a system directory, such that
# the compiler doesn't generate warnings in Google Test headers.
CPPFLAGS += -isystem $(GTEST_DIR)/include

# Flags passed to the C++ compiler.
CXXFLAGS += -g -Wall -Wextra -pthread

# All Google Test headers.  Usually you shouldn't change this
# definition.
GTEST_HEADERS = $(GTEST_DIR)/include/gtest/*.h \
                $(GTEST_DIR)/include/gtest/internal/*.h

GTEST_SRCS_ = $(GTEST_DIR)/src/*.cc $(GTEST_DIR)/src/*.h $(GTEST_HEADERS)

all: cgen test_runner

run_test: test_runner
	./$^

# For simplicity and to avoid depending on Google Test's
# implementation details, the dependencies specified below are
# conservative and not optimized.  This is fine as Google Test
# compiles fast and for ordinary users its source rarely changes.
gtest-all.o : $(GTEST_SRCS_)
	$(CXX) $(CPPFLAGS) -I$(GTEST_DIR) $(CXXFLAGS) -c \
            $(GTEST_DIR)/src/gtest-all.cc

gtest_main.o : $(GTEST_SRCS_)
	$(CXX) $(CPPFLAGS) -I$(GTEST_DIR) $(CXXFLAGS) -c \
            $(GTEST_DIR)/src/gtest_main.cc

gtest.a : gtest-all.o
	$(AR) $(ARFLAGS) $@ $^

gtest_main.a : gtest-all.o gtest_main.o
	$(AR) $(ARFLAGS) $@ $^

${OUTPUT}:	cgen
	@rm -f ${OUTPUT}
	./mycoolc  example.cl >example.output 2>&1 

cgen:	${OBJS} ${LIBS}
	${CC} ${CFLAGS} ${OBJS} ${LIB} -o cgen

${TESTS}: ${TESTS_SRC}
	${CC} ${CFLAGS} ${CPPFLAGS} -c $<

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

%.d: %.cc ${SRC}
	${SHELL} -ec '${DEPEND} $< | sed '\''s/\($*\.o\)[ :]*/\1 $@ : /g'\'' > $@'

-include ${CFIL:.cc=.d}

test_runner: ${TESTS} gtest_main.a
	${CXX} -pthread $^ -o $@