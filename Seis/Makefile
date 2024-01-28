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

all: src/Seis.f90
	$(FORTRAN_COMPILER) -O2 -g \
		-o bin/Seis.exe \
		src/Seis.f90

clean:
	rm -f bin/Seis.exe *.mod
