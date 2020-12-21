# modernfortran-libs
Object Based modern fortran library

## A fortran I/O library with routines similar to the GNU Octave's dlmread
In GNU Octave, functions such as dlmread and dlmwrite can be used to read text files. 
The *mod_io.f90* is a module that simulates the behavior of these functions.

## Dependencies
Basically, there are no other dependencies since it is written only in fortran.
It can be used by compiling and linking the f90 file under src.

If you uses a *CMake* to build and install this libraries, Doxygen will help you to create the manual as well.

`make doxygen`
