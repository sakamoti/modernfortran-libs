# modernfortran-libs
Object Based modern fortran library

## A fortran I/O library with routines similar to the GNU Octave's dlmread
In GNU Octave, functions such as dlmread and dlmwrite can be used to read text files. 
The *mod_io.f90* is a module that simulates the behavior of these functions.

*mod_doubly_linkedlist.f90* is a doubly linked list module which can list up any data types.

## Dependencies
Basically, there are no other dependencies since it is written only in fortran.
It can be used by compiling and linking the f90 file under src.

If you uses a *CMake* to build and install this libraries, Doxygen will help you to create the manual as well.

`make doxygen`

# Usage

## io module
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
## doubly_linkedlist module

Detail example is shown in the `app/test_doubly_linkedlist.f90`.

```fortran
program usage5()
    use iso_fortran_env
    use doubly_linkedlist 
    implicit none
    type(t_list) :: list !linked list data type
    type(t_elementptr) :: as_array
    procedure(list_sort_func) :: sortfun
    integer :: i,n
    ! generate new list
    n=5
    do i=1,n
      call list%append(i)
    end do
    print *, "Before sort"
    call list%showall()
    call list%sort(sortfun)
    print *, "After sort"
    call list%showall()
    contains
      function sortfun(one,two,passdata)result(res)
        class(*),intent(in) :: one,two
        class(*),intent(in),optional :: passdata
        logical :: res
        res=.false.
        select type(one)
        type is(integer)
          select type(two)
          type is(integer)
            if(one>two) res=.true.
          end select
        end select
      end function
end subroutine
```
