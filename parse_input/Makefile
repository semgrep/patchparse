TARGET=parse_input

LEXER_SOURCES = clexer.mll
PARSER_SOURCES = cparser2.mly

SRC=ast.ml ast0.ml ce.ml parse_error.ml \
$(LEXER_SOURCES:.mll=.ml) $(PARSER_SOURCES:.mly=.ml) \
small.ml diff.ml init.ml

SYSLIBS=str.cma unix.cma
LIBS=../globals/globals.cma

INCLUDES=-I ../globals


#The Caml compilers.
#for warning:  -w A
#for profiling:  -p -inline 0   with OCAMLOPT
OCAMLCFLAGS = -g
OCAMLC =ocamlc$(OPTBIN) $(OCAMLCFLAGS) $(INCLUDES)
OCAMLOPT = ocamlopt$(OPTBIN) $(OPTFLAGS) $(INCLUDES)
OCAMLDEP = ocamldep$(OPTBIN) $(INCLUDES)
OCAMLLEX=ocamllex$(OPTBIN) #-ml
OCAMLYACC=ocamlyacc -v


LIB=$(TARGET).cma
OPTLIB=$(LIB:.cma=.cmxa)

GENERATED= $(LEXER_SOURCES:.mll=.ml) \
	   $(PARSER_SOURCES:.mly=.ml) $(PARSER_SOURCES:.mly=.mli)
OBJS = $(SRC:.ml=.cmo)
OPTOBJS = $(SRC:.ml=.cmx)

all: $(LIB)
all.opt: $(OPTLIB)

$(LIB): $(GENERATED) $(OBJS)
	$(OCAMLC) -a -o $(LIB) $(OBJS)

$(OPTLIB): $(GENERATED) $(OPTOBJS)
	$(OCAMLOPT) -a -o $(OPTLIB) $(OPTOBJS)

clean::
	rm -f $(LIB)  $(OPTLIB) $(LIB:.cma=.a) $(TARGET).top


.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(OCAMLC) -c $<

.mli.cmi:
	$(OCAMLC) -c $<

.ml.cmx:
	$(OCAMLOPT) -c $<




# clean rule for others files
clean::
	rm -f *.cm[iox] *.o *.annot
	rm -f *~ .*~ #*#

depend:
	$(OCAMLDEP) *.mli *.ml > .depend

$(LEXER_SOURCES:.mll=.ml) :	$(LEXER_SOURCES)
	$(OCAMLLEX) $(LEXER_SOURCES)

$(PARSER_SOURCES:.mly=.ml) $(PARSER_SOURCES:.mly=.mli) : $(PARSER_SOURCES)
	$(OCAMLYACC) $(PARSER_SOURCES)

distclean::
	rm -f .depend

.depend:
	$(OCAMLDEP) *.mli *.ml > .depend

-include .depend

clexer.ml: clexer.mll
cparser2.ml: cparser2.mly clexer.mll
cparser2.mli: cparser2.mly clexer.mll
