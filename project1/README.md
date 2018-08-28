# Project 1

## Compilation
Use CMake, or compile manually.

### CMake:
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
