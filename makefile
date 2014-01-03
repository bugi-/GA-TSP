main: sizes.o helper_functions.o TSP_functions.o ga_functions.o main.f90
	gfortran sizes.o TSP_functions.o ga_functions.o helper_functions.o -o main -O3 main.f90
	rm *.o
	rm *.mod
	
openmp: sizes.o helper_functions.o TSP_functions.o ga_functions.o main.f90
	gfortran sizes.o TSP_functions.o ga_functions.o helper_functions.o -o main -fopenmp -O3 main.f90
	rm *.o
	rm *.mod

main_debug: sizes.o helper_functions.o TSP_functions.o ga_functions.o main.f90
	gfortran sizes.o TSP_functions.o ga_functions.o helper_functions.o -o main -pg -Wall -fcheck=all main.f90
	
test: sizes.o helper_functions.o TSP_functions.o ga_functions.o main.f90
	gfortran sizes.o TSP_functions.o ga_functions.o helper_functions.o -g -Wall -fcheck=all -o test test.f90
	rm *.o
	rm *.mod
	
sizes.o:
	gfortran -c sizes.f90

helper_functions.o:
	gfortran -c helper_functions.f90
	
TSP_functions.o:
	gfortran -c TSP_functions.f90

ga_functions.o:
	gfortran -c ga_functions.f90
