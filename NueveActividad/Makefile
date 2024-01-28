# ============================================================================
# Name        : Makefile
# Author      : 
# Version     :
# Copyright   : Your copyright notice
# Description : Makefile for Hello World in Fortran
# ============================================================================

.PHONY: all clean

# Change this line if you are using a different Fortran compiler
FORTRAN_COMPILER = gfortran

all: src/NueveActividad.f90
	$(FORTRAN_COMPILER) -O2 -g \
		-o bin/NueveActividad.exe \
		src/NueveActividad.f90

clean:
	rm -f bin/NueveActividad.exe *.mod
