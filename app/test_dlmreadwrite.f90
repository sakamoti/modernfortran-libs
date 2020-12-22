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
