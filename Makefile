#***************************************************************************
#   Copyright (C) 2005                                                    *
#     Philippe Bidinger (Philippe.Bidinger@inrialpes.fr)                  *
#     David Teller (D.O.Teller@sussex.ac.uk)                              *
#                                                                         *
#   This program is free software; you can redistribute it and/or modify  *
#   it under the terms of the GNU Library General Public License as       *
#   published by the Free Software Foundation; either version 2 of the    *
#   License, or (at your option) any later version.                       *
#                                                                         *
#   This program is distributed in the hope that it will be useful,       *
#   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
#   GNU General Public License for more details.                          *
#                                                                         *
#   You should have received a copy of the GNU Library General Public     *
#   License along with this program; if not, write to the                 *
#   Free Software Foundation, Inc.,                                       *
#   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
#**************************************************************************)

##
#
# File : Makefile
#
# Author : David Teller
#
# Makefile for the chalk machine.
#
# To build the chalk machine, please use
#   make config chalk
#  (bytecode version)
# or
#   make config chalk.opt
#  (native version)
#
# If you proceed to a fundamental change, be sure to reinvoke
#   make config
#
# $Log: Makefile,v $
# Revision 1.1.1.1  2005/10/03 14:54:36  formel
# creation of chalk2
#
# Revision 1.7  2005/10/02 16:26:09  formel
# *** empty log message ***
#
# Revision 1.6  2005/07/30 11:38:31  formel
#
# Removed un-consumption bug in the handling of join triggers.
#
# Revision 1.5  2005/07/29 18:14:43  formel
# Fixing CVS error.
#
# Revision 1.4  2005/07/29 15:33:55  formel
#
# Both styles of trigger-handling now compile.
#
# Revision 1.3  2005/07/26 13:35:13  formel
#
# Working on the runtime.
#
# Revision 1.2  2005/07/22 15:17:30  formel
# Fixed dependency handling.
#
# Revision 1.1.1.1  2005/07/22 14:39:35  formel
# Project recreated as chalk, in a manner more compliant with GNU coding standards.
#
#
#
#

OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLDEP=ocamldep
OCAMLDSORT=ocamldsort
#
# src/single for the original version
# src/join for the version with join patterns
#
DIRECTORIES=src src/single
INCLUDES=$(foreach dir, $(DIRECTORIES), -I $(dir) )
OCAMLFLAGS=-g -vmthread unix.cma threads.cma $(INCLUDES)
OCAMLOPTFLAGS=-thread unix.cmxa threads.cmxa  $(INCLUDES)
OCAMLLEX=ocamllex
OCAMLYACC=ocamlyacc


all:chalk

include depend

include linkdep


CHALK_OBJS_BYTECODE=$(patsubst %.ml, %.cmo, $(CHALK_ORDERED_SOURCE))
CHALK_OBJS_NATIVE=$(patsubst %.ml, %.cmx, $(CHALK_ORDERED_SOURCE))


chalk: $(CHALK_ORDERED_ML)
	@echo $(CHALK_ORDERED_ML)
	$(OCAMLC) -o chalk $(OCAMLFLAGS) $(CHALK_ORDERED_ML)

chalk.opt: $(CHALK_OBJS_NATIVE)
	$(OCAMLOPT) -o chalk.opt $(OCAMLOPTFLAGS) $(CHALK_OBJS_NATIVE)

src/lexer.ml: src/lexer.mll
	cd src && $(OCAMLLEX) lexer.mll

src/parser.ml: src/parser.mly
	cd src && $(OCAMLYACC) parser.mly 

# Common rules
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(OCAMLC) $(OCAMLFLAGS) -c $<

%.cmi: %.mli
	$(OCAMLC) $(OCAMLFLAGS) -c $<

.mli.cmi:
	$(OCAMLC) $(OCAMLFLAGS) -c $<

.ml.cmx:
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c $<

# Clean up
clean:
	rm -f chalk chalk.opt
	rm -f src/*.cm[iox]
	rm -f src/single/*.cm[iox]
	rm -f src/join/*.cm[iox]
	rm -f src/lexer.ml src/parser.ml

distclean: clean unconfig


# Dependencies
# depend:
# 	cd src && $(OCAMLDEP) $(INCLUDES) *.mli *.ml > ../.depend

config: src/lexer.ml src/parser.ml unconfig 

unconfig:
	\rm -f depend linkdep; $(MAKE) reconfig


reconfig: depend linkdep

depend: 
	$(OCAMLDEP) $(INCLUDES) $(foreach dir, $(DIRECTORIES), $(wildcard $(dir)/*.ml) $(wildcard $(dir)/*.mli))  > depend 


linkdep:
	@echo CHALK_ORDERED_ML=$(shell $(OCAMLDSORT) $(INCLUDES) -byte $(foreach dir, $(DIRECTORIES), $(wildcard $(dir)/*.ml))) > linkdep

test:
	@echo "mli" $(CHALK_ORDERED_MLI) 
	@echo "ml" $(CHALK_ORDERED_ML)

