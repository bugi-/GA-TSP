main: sizes.o functions.o ga_functions.o main.f90
	gfortran sizes.o functions.o ga_functions.o -o main -Wall -O3 main.f90
	rm *.o
	rm *.mod
	
main_openmp: sizes.o functions.o ga_functions.o main.f90
	gfortran sizes.o functions.o ga_functions.o -o main -fopenmp -Wall -O3 main.f90
	rm *.o
	rm *.mod

main_debug: sizes.o functions.o ga_functions.o main.f90
	gfortran sizes.o functions.o ga_functions.o -o main -pg -Wall -fcheck=all main.f90
	
test: sizes.o functions.o ga_functions.o test.f90
	gfortran sizes.o functions.o ga_functions.o -g -Wall -fcheck=all -o test test.f90
	rm *.o
	rm *.mod
	
sizes.o:
	gfortran -pg -c sizes.f90

functions.o:
	gfortran -pg -c functions.f90

ga_functions.o:
	gfortran -pg -c ga_functions.f90
