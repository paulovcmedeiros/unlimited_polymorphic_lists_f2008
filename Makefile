# Copyright (C) 2017 Paulo V. C. Medeiros
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program.  If not, see <http://www.gnu.org/licenses/>.
# ======================================================================
# Makefile adapted from:
#     Makefile-Fortran: A Makefile for a simple Fortran project
#     Copyright (C) 2009 Davide Cesari, dcesari <at> arpa.emr.it
#     Available at 
#     www.webalice.it/o.drofa/davide/makefile-fortran/makefile-fortran.html
#     Distributed according to the GNU GPL license.
#     This makefile works with the GNU make command, the one find on
#     GNU/Linux systems and often called gmake on non-GNU systems
#
# ======================================================================
# Declarations
# ======================================================================
# The compiler
FC = gfortran
# Required flags: 
FCFLAGS_for_gfortran=-std=f2008 -pedantic
FCFLAGS_for_ifort=-stand f08
# Shared memory parallelization
#FCFLAGS_for_ifort += -qopenmp -parallel
#FCFLAGS_for_gfortran += -fopenmp
#FCFLAGS_for_nagfor += -openmp
# Setting correct compiler options and enabling preprocessor
ifeq ($(FC),ifort)
  FCFLAGS=$(FCFLAGS_for_ifort)
  FPP = fpp 
else ifeq ($(FC),gfortran)
  FCFLAGS=$(FCFLAGS_for_gfortran)
  FPP = cpp 
else ifeq ($(FC),nagfor)
  FCFLAGS=$(FCFLAGS_for_nagfor)
  FPP = fpp 
endif

# Aliases
current_dir = $(shell pwd)
main_src_dir = $(current_dir)
general_include_dir = $(main_src_dir)/include

# Folders containing needed sources
vpath %.inc $(general_include_dir)
vpath %.inc $(wildcard $(general_include_dir)/**)
FCFLAGS += -I$(general_include_dir)

# List of targets to be built within the package
MAIN_SRC = upl_mod_demo.f90 
MAIN_SRC_OBJ = $(patsubst %.f90, %.o, $(MAIN_SRC))
MAIN_EXE = $(patsubst %.f90, %.x, $(MAIN_SRC))
# First rule: "make" will build this rule if no target is specified
all: $(MAIN_EXE)

# Objects
OBJ := storage_size_wrappers_mod.o aux_constants_and_types_mod.o \
       unlimited_polymorphic_lists_mod.o

# Rules to make the main targets individually
main: $(MAIN_EXE)
# Rules for dependencies
storage_size_wrappers_mod.o: aux_constants_and_types_mod.o
unlimited_polymorphic_lists_mod.o: generic_array_append.inc \
                                   generic_fmtd_unlim_poly_LL_nodeval.inc \
                                   aux_constants_and_types_mod.o \
                                   storage_size_wrappers_mod.o
$(MAIN_SRC_OBJ): $(OBJ) 
# ======================================================================
# General rules
# ======================================================================
$(MAIN_EXE): $(MAIN_SRC_OBJ) $(OBJ)
	$(FC) $(FCFLAGS) $(LDFLAGS) $^ $(LIBS) -o $@

%.o: %.f90
	$(FC) $(FCFLAGS) -$(FPP) $(FPPFLAGS) -c $<

# Utility targets
.PHONY: all clean veryclean print

clean:
	rm -f *.o *.mod *.MOD
	find . -name "*.pyc" -type f -delete
	find . -name "__pycache__" -type d -delete
veryclean: clean
	rm -f *~ $(MAIN_EXE)
print-%:
	@echo '$*=$($*)'
