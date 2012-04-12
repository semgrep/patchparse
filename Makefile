# Copyright 2012, INRIA, Julia Lawall
#
# Patchparse is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, according to version 2 of the License.
#
# Patchparse is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Patchparse.  If not, see <http://www.gnu.org/licenses/>.
#
# The authors reserve the right to distribute this or future versions of
# Patchparse under other licenses.


TARGET=patchparse

SRC=main.ml

SYSLIBS=str.cma unix.cma $(SEXPSYSCMA)
OPTSYSLIBS=$(SYSLIBS:.cma=.cmxa)
LIBS=globals/globals.cma \
 get_input/get_input.cma \
 parse_input/parse_input.cma \
 generalize_diffs/generalize_diffs.cma \
 select_diffs/select_diffs.cma \
 eq_classes/eq_classes.cma \
 output/output.cma
OPTLIBS=$(LIBS:.cma=.cmxa)

#used for clean: and depend: and a little for rec & rec.opt
MAKESUBDIRS=globals \
 get_input parse_input generalize_diffs select_diffs eq_classes output
INCLUDEDIRS=globals \
 get_input parse_input generalize_diffs select_diffs eq_classes output


##############################################################################
# Generic variables
##############################################################################

INCLUDES=$(INCLUDEDIRS:%=-I %)

OBJS=    $(SRC:.ml=.cmo)
OPTOBJS= $(SRC:.ml=.cmx)

EXEC=$(TARGET)
OPTEXEC=$(TARGET).opt

##############################################################################
# Generic ocaml variables
##############################################################################

OCAMLCFLAGS= -g

# for profiling add  -p -inline 0
# but 'make forprofiling' below does that for you.
# This flag is also used in subdirectories so don't change its name here.
# To enable backtrace support for native code, you need to put -g in OPTFLAGS
# to also link with -g, but even in 3.11 the backtrace support seems buggy so
# not worth it.
OPTFLAGS=
# the following is essential for Coccinelle to compile under gentoo
# but is now defined above in this file

OCAMLC=ocamlc$(OPTBIN) $(OCAMLCFLAGS)  $(INCLUDES)
OCAMLOPT=ocamlopt$(OPTBIN) $(OPTFLAGS) $(INCLUDES)
OCAMLLEX=ocamllex #-ml # -ml for debugging lexer, but slightly slower
OCAMLYACC=ocamlyacc -v
OCAMLDEP=ocamldep $(INCLUDES)


all: rec $(EXEC)
opt: rec.opt $(OPTEXEC)

native: rec.opt $(OPTEXEC)

rec:
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i all; done 

rec.opt:
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i all.opt; done 

$(EXEC): $(OBJS) $(LIBS)
	$(OCAMLC) -o $(EXEC) $(SYSLIBS) $(LIBS) $(OBJS)


$(OPTEXEC): $(OPTOBJS) $(OPTLIBS)
	$(OCAMLOPT) -o $(OPTEXEC) $(OPTSYSLIBS) $(OPTLIBS) $(OPTOBJS)


.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(OCAMLC) $(INCLUDE_PATH) -c $<

.mli.cmi:
	$(OCAMLC) $(INCLUDE_PATH) -c $<

.ml.cmx:
	$(OCAMLOPT) $(INCLUDE_PATH) -c $<

# clean rule for others files
clean::
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i clean; done 
	rm -f *.cm[iox] *.o 
	rm -f *~ .*~ #*# 
	rm -f .depend

depend: 
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i depend; done
	$(OCAMLDEP) $(INCLUDE_PATH) *.mli *.ml > .depend

.depend:
	$(OCAMLDEP) $(INCLUDE_PATH) *.mli *.ml > .depend

include .depend

release:
	$(MAKE) -C tools licensify
	tools/licensify
	set -e; for i in $(MAKESUBDIRS); do cd $$i ; ../tools/licensify; done 
	$(MAKE) -C . clean
	/bin/rm -r -f .svn */.svn
