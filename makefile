main:sizes.o functions.o ga_functions.o main.f90
	gfortran sizes.o functions.o ga_functions.o -o main -O3 main.f90
	rm *.o
	rm *.mod

test: sizes.o functions.o ga_functions.o test.f90
	gfortran sizes.o functions.o ga_functions.o -g -Wall -fcheck=all -o test test.f90
	rm *.o
	rm *.mod

sizes.o:
	gfortran -c sizes.f90

functions.o:
	gfortran -c functions.f90

ga_functions.o:
	gfortran -c ga_functions.f90
