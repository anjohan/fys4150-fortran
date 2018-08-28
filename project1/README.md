# Project 1

## Compilation
Use CMake, or compile manually.

The program asks for `log10(n)`. A value of `2` means that 100 points will be used.

### CMake
```
mkdir build
cd build
cmake ..
make
cd ..
./build/example
```

### Manually
Something along the lines of
```
gfortran -O3 -Wall -o example *.f90
./example
```

## Plotting
```
gnuplot plot.gpi
```
