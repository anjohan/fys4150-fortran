FC = caf
FFLAGS = -Wall -O3

all: example

%: ising.o %.o
	$(FC) $(FFLAGS) -o $@ $^
%.o: %.f90 Makefile
	$(FC) $(FFLAGS) -c $< -o $@

clean:
	rm -rf example *.o *.mod
