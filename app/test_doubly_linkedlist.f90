module mytype
  use iso_fortran_env
  use doubly_linkedlist 
  implicit none
  public
  type mydata
    character(4) :: txt="hoge"
    integer :: i = 2
  end type
  contains
    subroutine usage1()
      !<@brief t_list usage sample(add integer, array like usage)
      type(t_list) :: list,list_initial !define linked list data type
      type(t_elementptr),dimension(:),allocatable :: as_array
      class(*),pointer :: elm => null()
      integer :: i,n
      print *,"!***** BEGIN USAGE1 *******"
      ! generate new list
      n=10
      do i=1,n
        call list%append(i)
      end do
      !---
      list_initial=list !save initial data
      !---
      print *, "Show all elements in the list"
      call list%showall()
      print '(1X,A,I5,A)', "Before 'delall' method, exist",list%count()," elements."
      !---
      ! delete all elements
      call list%delall()
      print '(1X,A,I5,A,/)', "After 'delall' method, no elements in the list. Now exist",list%count()," elements."
      call list%showall()
      !---
      ! restore initial elements
      list=list_initial
      !---
      ! list to array-like t_elementptr type
      ! then, each element is doubled.
      print *,"How to treat list like array (each element is doubled)"
      as_array = list%listarray()
      do i=1,list%count()
        elm => as_array(i)%getobj()
        select type(elm)
        type is(integer)
          elm = elm *2
        end select
      end do
      call list%showall()
      !---
      ! destoroy exist elements and restore initial elements
      print *, "delete 3 to 8 elements"
      list=list_initial
      deallocate(as_array)
      as_array = list%listarray()
      ! delete 3 to 8th element
      do i=3,8
        call list%delete(as_array(i))
      end do
      call list%showall()
      print *,"!***** END USAGE1 *******"
    end subroutine
    subroutine usage2()
      !<@brief t_list usage sample(add different type)
      type(t_list) :: list,list_initial !define linked list data type
      type(t_elementptr) :: elo ! object to treat a lement
      type(mydata) :: udt ! user defined data type
      integer :: i
      print *,"!***** BEGIN USAGE2 *******"
      print *,"! Any data type can be stored."
      print *,"! user defined type can't print data.(printed as 'unrecognised item' as default)."
      call list%append((2.0,1.0)) !float
      call list%append((2d0,1d0)) !double
      call list%append(.TRUE.)    !logical
      call list%append(udt) !user defined type
      call list%append("character") !character
      call list%append(4d0)       !scalar double
      call list%showall()
      print *,"! user defined type can be printed (if user defined print procedure is passed)."
      call list%showall(showfun)
      print *,"! delete 3rd object"
      call elo%init(list) !initialize elo object
      call elo%next()
      call elo%next()
      write(output_unit,'(1x,"! 3rd element of list :")',advance='no')
      call list%show(elo)
      call list%delete(elo)
      call list%showall(showfun)
      !--
      call elo%tail()
      write(output_unit,'(1x,"! and delete tail element of list :")',advance='no')
      call list%show(elo)
      call list%delete(elo)
      call list%showall(showfun)
      !-----
      write(output_unit,'(1x,"! pointer does not break if you apply the oversize roop :")')
      call elo%tail()
      do i=1,list%count()+2
        call list%show(elo)
        call elo%prev()
      end do
      print *,"!***** END USAGE2 *******"
    end subroutine
    subroutine usage3()
      type(t_list) :: list !define linked list data type
      type(t_elementptr) :: as_array
      procedure(list_sort_func) :: sortfun
      integer :: i,n
      print *,"!***** BEGIN USAGE3 (sort test)*******"
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
      print *,"!***** END USAGE3 *******"
    end subroutine
    subroutine usage4()
      type(t_list) :: list !define linked list data type
      type(t_elementptr) :: as_array
      procedure(list_apply_proc) :: aplyfun
      integer :: i,n
      print *,"!***** BEGIN USAGE4 (apply test)*******"
      ! generate new list
      n=5
      do i=1,n
        call list%append(i)
      end do
      print *, "Before apply"
      call list%showall()
      call list%apply(aplyfun)
      print *, "After apply"
      call list%showall()
      print *,"!***** END USAGE4 *******"
    end subroutine
    subroutine usage5()
      type(t_list),pointer :: dp=>null(),dp_copy=>null()
      type(t_elementptr) :: elpt
      print *,"!***** BEGIN USAGE5 *******"
      allocate(dp,dp_copy)
      call dp%append((2d0,1d0)) !double
      call dp%append(.TRUE.)    !logical
      call elpt%init(dp)
      dp_copy=dp !user defined assignment
      deallocate(dp) !auto deallocation
      deallocate(dp_copy)!auto deallocation
      print *,"!***** END USAGE5 *******"
    end subroutine
    !obj_showに渡す関数の例
    subroutine showfun(obj,passdata,fid)
      class(*),intent(in) :: obj
      class(*),intent(in),optional :: passdata
      integer,intent(in),optional :: fid
      integer :: fileid
      if(present(fid))then
        fileid=fid
      else
        fileid=output_unit
      endif
      select type(obj)
      type is(mydata)
        write(fid,'(1X,"UserDefinedType:(",A,",",I2")")') obj%txt,obj%i
      end select
    end subroutine
end module
program test
  use iso_fortran_env
  use doubly_linkedlist 
  use mytype
  implicit none
  !test rouitne
  call usage1() ! add integer , array like usage
  call usage2() ! add different type objects and basic usage
  call usage3() ! sort test
  call usage4() ! apply test
  print*,compiler_version()
  print*,compiler_options()
end program
!-------------------------------------------------------
  !sort function
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
  !apply routine
  subroutine aplyfun(obj,passdata)
    class(*),intent(inout) :: obj
    class(*),intent(in),optional :: passdata
    select type(obj)
    type is(integer)
      obj=obj*3 !mod(obj,3)
    end select
  end subroutine
