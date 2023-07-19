
all:
	gfortran -o main pipe_module.f90 test_basic.f90  
	gfortran -o dscal pipe_module.f90 test_dscal.f90

.PHONY: clean
clean:
	rm -f dscal
	rm -f *.o