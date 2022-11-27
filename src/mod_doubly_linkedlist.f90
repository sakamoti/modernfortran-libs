!>@file Mod_List.f90
!> Doubly linked list module using unlimited polymorphic class.
!>
!-------------------------------------------------------------------
!>@file mod_doubly_linkedlist.f90
!>@brief Doubly linked list module using unlimited polymorphic class
!>
!> About *doubly_linkedlist* module
!> ==========================
!> Doubly linked list using unlimited polymorphic object pointer.
!> Doubly linked list which can hold any predefined data type or user defined type.@n
!>
!> * Public type
!>   name of type | commentary
!>   -------------|-------------------------
!>   t_list        | Object holding doubly linked list structure and data.
!>   t_elementptr   | Objects for external manipulation of list elements.
!>   
!> * Public interface (these function may be defined and passed by user)
!>   interfadce      | commentary
!>   ----------------|-------------------------
!>   list_sort_func  | sort linked list element
!>   list_apply_proc | this subroutine is apply all elements
!>   obj_show_proc   | print routine. this is useful to show user defined type.
!> .
!> 
!> Coding Policy
!> ==========================
!> Uses the Object Oriented Programing model incorporated in fortran 2003.
!> Encapsulation is taken into consideration, so that the list structure cannot be
!> directly modified from the outside. Therefore, list operations should be
!> performed throuth public methods.
!>
!> Example Usage
!> ==========================
!>@code
!>program test
!>  use doubly_linkedlist   !use this 'doubly_linkedlist' module
!>  implicit none
!>  type(t_list)      :: samplelist
!>  type(t_elementptr) :: element_ptr
!>
!>  !... process begin ...
!>  call samplelist%append(2d0)    !append element(real64) in the *samplelist*
!>  call samplelist%append(.TRUE.) !append element(logical) in the *samplelist*
!>  call samplelist%showall()      !show all elements in the *samplelist*
!>  !... process end ...
!>end program
!>@endcode
!> 
module doubly_linkedlist
  !$ use omp_lib
  use iso_fortran_env
  implicit none
  !基本属性はプライベート
  private

  !公開型
  !public t_list           !リスト
  !public t_elementptr      !リスト要素を指すポインタを含むオブジェクト
  !公開サブルーチン
  public obj_show        !組み込み型の表示+オプションで無限多相型の表示
  !公開インターフェース
  public list_sort_func  !ソート関数のインターフェース
  public list_apply_proc !apply 関数のインターフェース
  public obj_show_proc   !無限多相性オブジェクト表示のインターフェース

  !---------
  !>@brief リスト要素オブジェクト
  !>
  !>モジュール内で使用されるリスト要素
  !>@note オブジェクト指向なのでfortran2003,2008の機能必須。
  type,private :: elm
    class(*)   ,pointer,private :: obj=>null() !<リスト要素の実体へのポインタ
    type(elm)  ,pointer,private :: nxt=>null() !<次要素へのリンク
    type(elm)  ,pointer,private :: bef=>null() !<前要素へのリンク
    class(t_list),pointer,private :: parent=>null() !<リストを保持する親オブジェクトへのポインタ
    contains
      final :: elm_final !<elm型のデストラクタ
      procedure,private :: elm_equal !<ユーザー定義代入操作(ルーチン実体)
      generic :: assignment(=) => elm_equal !<ユーザー定義代入操作(generic,assignment)
  end type
  !---------
  !>@brief リスト要素オブジェクト(外部からの操作機能付き)
  !>
  !>この型を利用する前にリスト要素を示すように初期化されている必要がある。
  !>initメソッドで初期化可能。
  type,public :: t_elementptr
    type(elm)  ,pointer,private :: pos => null()  !<リスト要素へのポインタ
    class(t_list),pointer,private :: parent=>null() !<親オブジェクトへのポインタ
    contains
      final :: t_elementptr_final !<t_elementptr型のデストラクタ
      procedure,non_overridable,public :: init    => t_elementptr_head !<指示先をリスト先頭に戻す
      procedure,non_overridable,public :: tail    => t_elementptr_tail !<指示先をリスト最後尾に
      procedure,non_overridable,public :: next    => t_elementptr_next !<指示先を一つ次に移す
      procedure,non_overridable,public :: prev    => t_elementptr_previous !<指示先を一つ前に移す
      procedure,non_overridable,public :: getobj  => t_elementptr_getobj   !<指示先の要素へのポインタを得る
  end type
  !---------
  !>@brief リンクリストを保持するオブジェクト。
  type,public :: t_list
    type(elm),pointer,private :: head=>null() !<リスト先頭へのポインタ
    integer,private :: num=0         !<リスト要素数
    contains
      final :: list_final !<t_list型のデストラクタ
      procedure,non_overridable,public :: append    => list_append  !<
        !<append a element (subroutine,impure elemental)
      procedure,non_overridable,public :: delete    => elm_delete !<
        !<delete a element (subroutine,impure elemental)
      procedure,non_overridable,public :: delall    => list_delall !<
        !<delete all elements (subroutine,pure elemental)
      procedure,non_overridable,public :: count     => list_count_elm !<
        !<count how many elements are there (subroutine,pure elemental)
      procedure,non_overridable,public :: showall   => list_showall !<
        !<show all elements (subroutine)
      procedure,non_overridable,public :: apply     => list_apply !<
        !<apply user defined routine in each elements (subroutine)
      procedure,non_overridable,public :: sort      => list_sort !<
        !<sort elements by using user defined routine (subroutine)
      procedure,non_overridable,public :: listarray => list_elem_pointer_array !<
        !<リスト要素を指す配列を作る(function)
      procedure,non_overridable,nopass,public:: show => elm_show !<
        !<指定された要素1つを表示(subroutine)
      procedure,non_overridable,private :: copy      => list_copy !<
        !<ユーザー定義代入操作の実体
      generic :: assignment(=) => copy !<
        !<ユーザー定義代入操作(リストのコピー)
  end type

  !----
  interface
    !>@brief リストをソートするための関数
    !>@param[in] one ソートで比較する要素1
    !>@param[in] two ソートで比較する要素2
    !>@param[in] passdata (optional)比較の為に使う追加データ
    !>@retval is_swap oneとtwoを入れ替えるときTRUE
    function list_sort_func(one,two,passdata)result(is_swap)
      class(*),intent(in) :: one,two
      class(*),intent(in),optional :: passdata
      logical :: is_swap
    end function
    !>@brief apply関数で与える関数の型
    !>@param[inout] obj 操作対象のオブジェクト(リスト要素)
    !>@param[in] passdata (optional)追加データが必要な時に使う
    subroutine list_apply_proc(obj,passdata)
      class(*),intent(inout) :: obj
      class(*),intent(in),optional :: passdata
    end subroutine
    !>@brief obj_showルーチンでユーザー定義型を表示する関数
    !>@param[in] obj 操作対象のオブジェクト(リスト要素)
    !>@param[in] passdata (optional)追加データが必要な時に使う
    !>@param[in] fid ファイルid
    subroutine obj_show_proc(obj,passdata,fid)
      class(*),intent(in) :: obj
      class(*),intent(in),optional :: passdata
      integer ,intent(in),optional :: fid
    end subroutine
  end interface
  contains
    !--------------------------------
    !>@brief t_elementptrが指し示すリスト要素を初期化する
    !>
    !>@param[inout] self t_elementptr型
    !>@param[in] list 親となるt_list型
    impure elemental subroutine t_elementptr_head(self,list) 
      class(t_elementptr),intent(inout) :: self
      class(t_list),intent(in),target :: list
      self%parent=>list
      if(associated(list%head))then
        self%pos=>list%head
      else
        self%pos=>null()
        !write(error_unit,*) "wornning!! This list doesn't have the head element."
      endif
    end subroutine
    !--------------------------------
    !>@brief t_elementptrが指し示すリスト要素を最後尾にする
    !>
    !>@param[inout] self t_elementptr型
    impure elemental subroutine t_elementptr_tail(self) 
      class(t_elementptr),intent(inout) :: self
      !integer :: i,n
      call t_elementptr_check_parent(self)
      if(.not.associated(self%pos)) return
      !n=self%parent%count()
      !do i=1,n
      do while(associated(self%pos%nxt))
        call self%next()
      enddo
    end subroutine
    !--------------------------------
    !>@brief t_elementptrが指し示す要素を一つ次に進める
    !>
    !>@param[inout] self 操作対象のt_elementptr型
    impure elemental subroutine t_elementptr_next(self)
      class(t_elementptr),intent(inout),target :: self
      call t_elementptr_check_parent(self)
      if(.not.associated(self%pos)) return
      if(associated(self%pos%nxt))then
        self%pos=>self%pos%nxt
      else
        !self%ipos_ptr=>self%head
      endif
    end subroutine
    !--------------------------------
    !>@brief t_elementptrが指し示す要素を一つ前に進める
    !>
    !>@param[inout] self 操作対象のt_elementptr型
    impure elemental subroutine t_elementptr_previous(self)
      class(t_elementptr),intent(inout),target :: self
      call t_elementptr_check_parent(self)
      if(.not.associated(self%pos)) return
      if(associated(self%pos%bef))then
        self%pos=>self%pos%bef
      else
        !self%ipos_ptr=>self%head
      endif
    end subroutine
    !--------------------------------
    !>@brief t_elementptrが指し示す要素を直に指すポインタを返す
    !>
    !>@param[in] self 操作対象のt_elementptr型
    !>@retval res 無限多相性のオブジェクトを示すポインタ
    function t_elementptr_getobj(self)result(res)
      class(t_elementptr),intent(in) :: self
      class(*),pointer :: res
      res=>null()
      if(.not.associated(self%pos))return
      if(.not.associated(self%pos%obj))return
      !allocate(res,source=self%pos%obj)
      res=>self%pos%obj
    end function
    !--------------------------------
    !>@brief t_elementptrが親リストを持つかどうかチェックする
    !>
    !> - 要素の関連付けなし。親リストの関連付けなし。:処理を中断。
    !> - 要素の関連付けなし。親リストの関連付けあり。:要素をリスト先頭に関連付け。
    !> - 要素の関連付けあり。親リストの関連付けなし。:警告を表示して続行。
    !> .
    !>@param[in] elpt 操作対象のt_elementptr型
    impure elemental subroutine t_elementptr_check_parent(elpt)
      class(t_elementptr),intent(inout) :: elpt
      if(.not.associated(elpt%pos))then
        if(associated(elpt%parent))then
          elpt%pos=>elpt%parent%head
        else
          stop "@t_elementptr_check_parent"
        endif
     !else
     !  if(.not.associated(elpt%pos%parent))then
     !    write(error_unit,*) &
     !      "warnning!! This t_elementptr doesn't have the parent type 'list'."
     !  endif
      endif 
    end subroutine
    !--------------------------------
    !>@brief t_elementptr型のデストラクタ
    !>
    !>@param[in] self t_elementptr型
    pure elemental subroutine t_elementptr_final(self)
      type(t_elementptr),intent(inout) :: self
      self%pos=>null()
      self%parent=>null()
    end subroutine
    !--------------------------------
    !>@brief イコールの演算子でリストの要素をコピーする
    !>
    !>@param[out] left イコールの左側
    !>@param[in]  right イコールの右側
    impure elemental subroutine elm_equal(left,right)
      class(elm),intent(out) :: left
      class(elm),intent(in)  :: right
      if(associated(right%obj))allocate(left%obj,source=right%obj)
      left%nxt=>right%nxt
      left%bef=>right%bef
      left%parent=>right%parent !要素のコピーだけ発生する場合,親は変えない
    end subroutine
    !--------------------------------
    !>@brief elm型のデストラクタ
    !>
    !>@param[in] self elm型
    impure elemental subroutine elm_final(self)
      type(elm),intent(inout) :: self
      !print*,"dealloc elm"
      if(associated(self%obj))deallocate(self%obj)
      self%nxt=>null()
      self%bef=>null()
      self%parent=>null()
    end subroutine
    !--------------------------------
    !>@brief elmptr型が示すオブジェクトを表示
    !>
    !>@param[inout] mydata t_elementptr型
    !>@param[in] showproc obj_show_procインターフェースで示される引数を持つ関数
    !>@param[in] passdata (optional)表示で使うオプションデータ
    !>@param[in] fid ファイルid
    subroutine elm_show(mydata,showproc,passdata,fid)
      class(t_elementptr),intent(inout) :: mydata
      procedure(obj_show_proc),optional :: showproc
      class(*),intent(in),optional :: passdata
      integer ,intent(in),optional :: fid
      if(.not.associated(mydata%pos))return
      call t_elementptr_check_parent(mydata)
      call obj_show(mydata%pos%obj,printobj=showproc,passdata=passdata,fid=fid)
    end subroutine
    !--------------------------------
    !>@brief t_list型の全要素を削除する
    !>
    !>@param[inout] self t_list型
    impure elemental subroutine list_delall(self)
      class(t_list),intent(inout) :: self
      type(elm),pointer :: tmp,ielm
      integer :: i
      i=0
      ielm=>self%head
      do
        if(associated(ielm))then
          !print*,"deallocate list contents of ",i
          i=i+1
          tmp=>ielm%nxt
          deallocate(ielm)
          ielm=>tmp
        else
          exit
        endif
      enddo
      !if(mod(i,100000)==1) print*,"list_delall",i
      self%num=0
      self%head=>null()
    end subroutine
    !--------------------------------
    !>@brief t_list型のデストラクタ
    !>
    !>@param[inout] self t_list型
    impure elemental subroutine list_final(self)
      type(t_list),intent(inout) :: self
      !type(elm),pointer :: tmp,ielm
      !integer,save :: icnt=0
      call self%delall()
     !icnt=icnt+1
     !if(mod(icnt,100000)==1)then
     !  print*,"list_final call",icnt
     !endif
    end subroutine
    !--------------------------------
    !>@brief t_list型への要素追加関数
    !>
    !>@param[inout] self t_list型
    !>@param[in] obj リスト先頭に要素(何でも良い)を追加する
    !>@param[inout] addloc (optinal)t_elementptrの指示先の次に要素objを追加する
    subroutine list_append(self,obj,addloc)
      class(t_list),intent(inout),target :: self
      class(*),intent(in) :: obj
      type(t_elementptr),intent(inout),optional :: addloc
      type(elm),pointer :: add,tmp
      allocate(elm::add)
      allocate(add%obj,source=obj)
      add%parent=>self !要素の親リストを示す
      if(present(addloc))then
        !list中のaddlocの次の位置にelmを挿入
        if(.not.associated(addloc%parent))then
          write(error_unit,*)"要素を追加するリストが指定されていません"
          write(error_unit,*)"データはリストに追加されませんでした。"
          return
        endif
        if(.not.associated(addloc%pos))then
          write(error_unit,*)"追加する場所のポインタが空です"
          write(error_unit,*)"データはリストに追加されませんでした。"
          return
        else
          !追加
          add%nxt=>addloc%pos%nxt
          addloc%pos%nxt=>add      !ここでaddlocのポインタ指示先が書き換えられる
          add%bef=>addloc%pos
        endif
      else
        !headに挿入
        if(.not.associated(self%head))then
          self%head=>add
        else
          tmp=>self%head
          self%head=>add
          self%head%nxt=>tmp
          tmp%bef=>self%head
        endif
      endif
      self%num=self%num+1
    end subroutine
    !--------------------------------
    !>@brief リストの全要素に関数を適用する
    !>
    !>@param[in] self t_list型
    !>@param[in] applyproc (list_apply_proc)インターフェースを持つユーザー定義ルーチン
    !>@param[in] passdata (optinal)applyprocルーチンで追加データを利用する場合に使用
    !>@param[in] parallel (optional)並列実行したいときに.TRUE.を指定
    !>@note Rのapply関数と同様の動作を意図して作成した。
    subroutine list_apply(self,applyproc,passdata,parallel)
      class(t_list),intent(inout) :: self
      procedure(list_apply_proc) :: applyproc
      class(*),intent(in),optional :: passdata
      logical,intent(in),optional :: parallel
      type(t_elementptr) :: ipt
      integer :: i
      logical :: do_para
      type(t_elementptr),dimension(:),allocatable :: temp

      do_para=.false.
      if(present(parallel))then
        if(parallel) do_para=.true.
      endif

      if(do_para)then
        !並列実行(OpenMP)
          temp=self%listarray()
          !$omp parallel 
          !$omp do
          do i=1,size(temp)
            !関数をリスト要素に適用
            call applyproc(temp(i)%pos%obj,passdata=passdata) 
          enddo
          !$omp end do
          !print *, "Hello! N =", omp_get_num_threads(), " and I am ", omp_get_thread_num()
          !$omp end parallel
      else
        !逐次実行
        call ipt%init(self)
        do i=1,self%num 
          !関数をリスト要素に適用
          call applyproc(ipt%pos%obj,passdata=passdata) 
          call ipt%next()
        enddo
      endif
    end subroutine
    !--------------------------------
    !>@brief リスト要素を全て表示する
    !>
    !>@param[in] self t_list型
    !>@param[in] showproc (optinal)ユーザー定義型を表示するルーチン
    !>@param[in] passdata (optional)表示に使う追加データ
    !>@param[in] fid ファイルid
    subroutine list_showall(self,showproc,passdata,fid)
      class(t_list),intent(in),target :: self
      procedure(obj_show_proc),optional :: showproc
      class(*),intent(in),optional :: passdata
      integer,intent(in),optional :: fid
      type(t_elementptr) :: ipt
      integer :: i,fileid
      if(present(fid))then
        fileid=fid
      else
        fileid=output_unit
      endif
      call ipt%init(self)
      do i=1,self%num 
        write(fileid,'(1X,"Item ",I0,"/",I0,";",2X)',advance="no") &
          i,self%num
        call self%show(ipt,showproc=showproc,passdata=passdata,fid=fileid)
        call ipt%next()
      enddo
    end subroutine
    !--------------------------------
    !>@brief リストから要素を削除する
    !>
    !>引数に与えたポインタが示す先の要素をリストから削除する。
    !>@param[in] self t_list型
    !>@param[inout] delelm リスト要素のポインタ
    subroutine elm_delete(self,delelm)
      class(t_list),intent(in),target :: self
      class(t_elementptr),intent(inout) :: delelm
      type(elm),pointer :: bef,nxt
      class(t_list),pointer :: plist
      !ポインタ指示先の確認
      if(.not.associated(delelm%pos))return
      if(.not.associated(delelm%parent,target=self))then
        !print*,"追加する場所はリストの要素ではありません"
        print*,"削除できませんでした。"
        return
      else
        !削除
        plist=>delelm%parent
        plist%num=plist%num-1
        bef=>delelm%pos%bef
        nxt=>delelm%pos%nxt
        if(associated(bef))then
          bef%nxt=>nxt
        else
          !リスト先頭を削除するのでheadの支持先を修正
          plist%head=>nxt
        endif
        if(associated(nxt))then
          nxt%bef=>bef
        endif
        deallocate(delelm%pos)
        delelm%pos=>nxt
      endif
    end subroutine
    !--------------------------------
    !>@brief リスト要素数を返す関数
    !>
    !>@param[in] self t_list型
    pure elemental function list_count_elm(self)result(n)
      class(t_list),intent(in) :: self
      integer :: n
      n=self%num
    end function
    !--------------------------------
    !>@brief リストのコピー(メモリを新しくアロケート)
    !>
    !>@param[in] right イコールの右
    !>@param[in] left  イコールの左
    impure elemental subroutine list_copy(left,right)
      class(t_list),intent(in) :: right
      class(t_list),intent(out) :: left
      type(t_elementptr) :: elpr
      integer :: i,n
      !コピー元の要素を指すelprを初期化
      call elpr%init(right)
      !リスト最後尾を指す状態にする
      call elpr%tail()
      !リストを一つずつ追加
      n=right%count()
      do i=1,n
        call left%append(elpr%pos%obj)
        call elpr%prev()
      enddo
    end subroutine
    !--------------------------------
    !>@brief バブルソートの実行
    !>
    !>@param[inout] self ソートを実行する
    !>@param[in] func list_sort_funcインターフェースを持つサブルーチン
    !>@param[in] passdata (optional)ソートで使う追加データ
    subroutine list_sort(self,func,passdata)
      class(t_list),intent(inout),target :: self
      procedure(list_sort_func) :: func
      class(*),intent(in),optional :: passdata
      class(elm),pointer :: ipt
      class(elm),pointer :: jpt
      class(*),pointer :: tmpptr
      !class(*),allocatable :: tmp
      !要素数が1以下ならsortしなくて良い
      if(self%num<=1)return
      ipt=>null()
      jpt=>null()
      !func(one,two)result(is_swap)
      !one,twoの２つの順番を入れ替えるときにis_swapがTRUEとなる
      ipt=>self%head
      do
        if(.not.associated(ipt))exit
        jpt=>ipt%nxt
        do
          if(.not.associated(jpt))exit
          !入れ替えのチェック
          if(func(ipt%obj,jpt%obj,passdata))then
            !入れ替えswap
            !allocate(tmp,source=ipt%obj)
            !call move_alloc(from=jpt%obj,to=ipt%obj)
            !call move_alloc(from=tmp,to=jpt%obj)
            !リンクの修正の方が早い
            tmpptr=>ipt%obj
            ipt%obj=>jpt%obj
            jpt%obj=>tmpptr
          endif
          !ポインタをすすめる
          jpt=>jpt%nxt
        enddo
        ipt=>ipt%nxt
      enddo
    end subroutine
    !--------------------------------
    !>@brief リスト要素を配列として扱うためのt_elementptr型配列の生成ルーチン
    !>
    !>@param[in] self リスト
    !>@retval res t_elementptr配列
    function list_elem_pointer_array(self)result(res)
      class(t_list),intent(in),target :: self
      type(t_elementptr),dimension(:),allocatable :: res
      type(elm),pointer :: tmp
      integer :: i
      allocate(res(self%num))
      if(self%num<=0)return
      call res(:)%init(self) !initialize elementary
      tmp=>self%head
      do i=1,self%num
        res(i)%pos=>tmp
        tmp=>tmp%nxt
      enddo
    end function
    !--------------------------------
    !>@brief 無限多相性オブジェクトを表示するルーチン
    !>
    !>@param[in] obj 要素
    !>@param[in] printobj (optional)obj_show_procインターフェースを持つ
    !>                  ユーザー定義型を表示する場合の手続き
    !>@param[in] passdata (optional)printobjで使う追加データ
    !>@param[in] fid (optional)ファイルid
    subroutine obj_show(obj,printobj,passdata,fid)
      class(*),intent(in)::obj
      procedure(obj_show_proc),optional::printobj
      class(*),intent(in),optional :: passdata
      integer ,intent(in),optional :: fid
      integer :: fileid
      !ファイルidの設定
      if(present(fid))then
        fileid=fid
      else
        fileid=output_unit
      endif
      !表示ルーチン
      select type(x=>obj)
      type is(integer(kind=int8))
        write(fileid,'(1X,G0)')x
      type is(integer(kind=int16))
        write(fileid,'(1X,G0)')x
      type is(integer(kind=int32))
        write(fileid,'(1X,G0)')x
      type is(integer(kind=int64))
        write(fileid,'(1X,G0)')x
      type is(real(kind=real32))
        write(fileid,'(1X,G0)')x
      type is(real(kind=real64))
        write(fileid,'(1X,G0)')x
      type is(real(kind=real128))
        write(fileid,'(1X,G0)')x
      type is(logical)
        write(fileid,'(1X,G0)')x
      type is(complex)
        write(fileid,'(1X,"(",G0,",",G0,")")')real(x),aimag(x)
      type is(complex(kind(0d0)))
        write(fileid,'(1X,"(",G0,",",G0,")")')real(x),aimag(x)
      type is(character(len=*))
        write(fileid,'(1X,G0)')x
      class default
        if(present(printobj))then
          call printobj(obj,passdata=passdata,fid=fid)
        else
          write(fileid,*)"unrecognised item"
        endif
      end select
    end subroutine
    !--------------------------------
end module
