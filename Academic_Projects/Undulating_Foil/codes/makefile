# This is the makefile for the plots.f90
#gfortran $(OBJECTS) -o main.exe
# 
FC = gfortran
FFLAGS = -g
LFLAGS = 
SOURCES = $(wildcard *.f90)
OBJECTS = $(subst .f90,.o,$(SOURCES))
MODS = $(wildcard mod*.f90)
MODULES = $(subst .f90,.mod,$(MODS))
LIBS = -lblas -llapack -o

.PHONY: clean

plot:
	gnuplot style.gnu;

output: compile
	./main.exe #-> output.txt
	exit

compile: $(MODULES) $(OBJECTS)
	$(FC) $(FFLAGS) $(OBJECTS) $(LIBS) main.exe

%.o: %.f90
	$(FC) $(FFLAGS) -c $<

%.mod: %.f90
	$(FC) $(FFLAGS) -c $<
	
clean:
	rm -f $(wildcard *.mod) $(OBJECTS)

help: 
	@echo "Main Files = " $(wildcard main.f90)
	@echo "Module Files = " $(wildcard mod*.f90)

gdb:
	$(FC) -g $(OBJECTS) $(LIBS) debug