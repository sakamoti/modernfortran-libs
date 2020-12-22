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

## Usage
`fortran type(t_dfstats)` is the type which contains double precision 2-dimensional array *xx*.
Once you read data from file using `dlmread` routine, you can use array as a drived type components. 
For example you can use `t_dfstats` type like below.
```fortran
program main
    use io
    implicit none
    integer :: i
    type(t_dfstats) :: a ! data is read into this type :: a%xx
    ! Input data from file
    call a%dlmread("test.dat",comment="#",sep=" ",skip=3) ! read data
    call a%showhead() ! show headdings
    do i=1,size(a%xx(:,1))
        print *,a%xx(i,:)
    enddo
    ! Output data to file
    call a%dlmwrite("output_test.dat",sep=":")
end program
```
