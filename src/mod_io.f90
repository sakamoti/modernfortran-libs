!>@file Mod_IO.f90
!>@brief Input/Output module like gnu Octave.
!>
! 
!>@brief Input/Output module@n
!>
!> about io module
!> ==========================
!> octave のdlmreadの動作を真似して作成した。@n
!> t_dfstats型の要素はユーザーが直接扱う事ができるものの、
!> 型束縛手続きを通して扱うことしか考慮していない。
!> そのため、型内部の要素は参照のみに留めることを強く勧める。
!>
!> public name | explanation
!> ------------|---------------
!> t_dfstats   | user defined data type
!>
!> example usage
!> ==========================
!>@code
!>   program main
!>     use io
!>     implicit none
!>     type(t_dfstats) :: a
!>     double precision,allocatable :: x(:,:)
!>     a%dlmread("test.dat","#"," ",skip=3) !read data
!>     call a%head()   !show head
!>   end program
!>@endcode
module io
  !$ use omp_lib
  use,intrinsic :: iso_fortran_env, only : dp => real64, &
    output_unit, error_unit, iostat_end, iostat_eor 
  implicit none
  private

  !>@brief type definition which contains data and statistics value
  type,public :: t_dfstats
    character(len=:),allocatable,public :: filename   !< input file name
    real(kind=dp),dimension(:,:),allocatable,public :: xx !< array of input data
    real(kind=dp),dimension(:)  ,allocatable,public :: average  !<average
    real(kind=dp),dimension(:)  ,allocatable,public :: variance !<
    real(kind=dp),dimension(:)  ,allocatable,public :: std      !<standard deviation
    real(kind=dp),dimension(:)  ,allocatable,public :: minvalue !<min value
    real(kind=dp),dimension(:)  ,allocatable,public :: maxvalue !<max value
    contains
      final :: destroy_t_dfstats          !< deallocate allocatable components
      ! copy data from array
      procedure,non_overridable,private :: array2t_dfstats_1d !<1 dimension
      procedure,non_overridable,private :: array2t_dfstats_2d !<2 dimension
      generic,public :: array2t_dfstats => array2t_dfstats_1d,array2t_dfstats_2d
           !< copy data from array to t_dfstats
      ! data reading procedures
      procedure,non_overridable,public  :: dlmread     !< data reading procedure
      procedure,non_overridable,public  :: dlmread_nosearch !<data reading procedure (faster version)
      procedure,non_overridable,private :: getnum      !< function getting value inside the 'dlmread'
      procedure,non_overridable,public  :: dlmwrite    !< data output procedure
      ! showing data
      procedure,non_overridable,public  :: showhead    !< write header (for debug purpose)
      ! evaluating stats value
      procedure,non_overridable,public  :: calave      !< calculate avarage
      procedure,non_overridable,public  :: calvar      !< calculate unbiased variance
      procedure,non_overridable,public  :: calstd      !< calculate standard deviation
      procedure,non_overridable,public  :: calmin      !< calculate min value
      procedure,non_overridable,public  :: calmax      !< calculate max value
  end type

  contains
    !--------------------------------------------------------
    !>@brief finalization of t_dfstats
    pure elemental subroutine destroy_t_dfstats(self)
      type(t_dfstats),intent(inout) :: self
      if(allocated(self%filename))then
        deallocate(self%filename)
      endif
      if(allocated(self%xx))then
        deallocate(self%xx)
      endif
      if(allocated(self%average))then
        deallocate(self%average)
      endif
      if(allocated(self%variance))then
        deallocate(self%variance)
      endif
      if(allocated(self%std))then
        deallocate(self%std)
      endif
      if(allocated(self%minvalue))then
        deallocate(self%minvalue)
      endif
      if(allocated(self%maxvalue))then
        deallocate(self%maxvalue)
      endif
    end subroutine
    !--------------------------------------------------------
    !>@brief copy 1d array to the element of t_dfstats xx
    !>
    !>@note 引数に指定したdfの要素は、サブルーチンが実行されると
    !>上書きして消されるので注意すること。
    subroutine array2t_dfstats_1d(df,array)
      class(t_dfstats),intent(inout) :: df  !<
      real(kind=dp),dimension(:),intent(in) :: array !<
      integer :: nrow
      call destroy_t_dfstats(df)
      nrow=size(array)
      allocate(df%xx(nrow,1))
      df%xx(:,1)=array(:)
    end subroutine
    !--------------------------------------------------------
    !>@brief 2次元配列からt_dfstats型を生成するルーチン
    !>
    !>引数に指定したdfの要素は、サブルーチンが実行されると
    !>上書きして消されるので注意すること。
    subroutine array2t_dfstats_2d(df,array)
      class(t_dfstats),intent(inout) :: df
      real(kind=dp),dimension(:,:),intent(in) :: array !<t_dfstatsに保存したい1次元配列
      call destroy_t_dfstats(df)
      allocate(df%xx,source=array)
    end subroutine
    !--------------------------------------------------------
    !>@brief print headdings (3 lines)
    !>
    subroutine showhead(self) 
      class(t_dfstats),intent(in) :: self
      integer,dimension(2) :: lowcol
      integer :: i,j
      if(.not.allocated(self%xx))then
        write(error_unit,*)"error : データフレームが読み込まれていません"
        write(error_unit,*)"関数を終了します"
        return
      endif
      lowcol=shape(self%xx)
      do i=1,min(3,lowcol(1))
        write(output_unit,'("row",i3.3,":")',advance="no")i
        do j=1,lowcol(2)
          write(output_unit,'(f10.4)',advance="no")self%xx(i,j)
        enddo
        write(output_unit,'(A)')
      enddo
    end subroutine
    !--------------------------------------------------------
    !>@brief 列毎または行毎のデータ最小値を求める
    !>
    !>@param[inout] self 計算したいデータ
    !>@param[in]    dir  1:列毎の最小、2:行毎の最小
    !>@param[out]   ier (optional)エラーフラグ。0なら正常。
    pure elemental subroutine calmin(self,dir,ier) 
      class(t_dfstats),intent(inout) :: self
      integer,intent(in),optional :: dir  !1:colwise、2:rowsize
      integer,intent(out),optional :: ier !<error flag. 0 means correct
      real(kind=dp),dimension(:),allocatable :: my_mindim
      integer,dimension(2) :: cols
      integer,dimension(2) :: iflag
      if(present(ier))ier=0 !エラーがなければ0
      if(present(dir))then
        if(dir==1)then
          !列方向の最小値
          iflag(1)=2 
          iflag(2)=1
        elseif(dir==2)then
          !行方向の最小値
          iflag(1)=1
          iflag(2)=2
        else
          !引数不正
          !write(error_unit,*)"dir should be '1' or '2'."
          if(present(ier))ier=-1 !エラーがなければ0
          return
        endif
      else
        !dirが与えられない時
        iflag(1)=2
        iflag(2)=1
      endif
      !
      cols=shape(self%xx) !行数、列数
      allocate(my_mindim(cols(iflag(1)))) !列数分の配列
      !行方向の和->列毎に和を集計することになる
      my_mindim=minval(self%xx,dim=iflag(2)) 
      !最小値
      if(allocated(self%minvalue))then
        deallocate(self%minvalue)
      endif
      self%minvalue=my_mindim
    end subroutine
    !--------------------------------------------------------
    !>@brief 列毎または行毎のデータ最大値を求める
    !>
    !>@param[inout] self 計算したいデータ
    !>@param[in]    dir  1:列毎の最大、2:行毎の最大
    !>@param[out]   ier (optional)エラーフラグ。0なら正常。
    pure elemental subroutine calmax(self,dir,ier) 
      integer,intent(in),optional :: dir !1:列毎の平均、2:行毎の平均
      class(t_dfstats),intent(inout) :: self
      integer,intent(out),optional :: ier    !エラーフラグ
      real(kind=dp),dimension(:),allocatable :: my_maxdim
      integer,dimension(2) :: cols
      integer,dimension(2) :: iflag
      if(present(ier))ier=0 !エラーがなければ0
      if(present(dir))then
        !dirが与えられた時
        if(dir==1)then
          !列方向の最大値
          iflag(1)=2 
          iflag(2)=1
        elseif(dir==2)then
          !行方向の最大値
          iflag(1)=1
          iflag(2)=2
        else
          !引数不正
          !write(*,*)"dir should be '1' or '2'."
          if(present(ier))ier=-1 !エラーがなければ0
          return
        endif
      else
        !dirが与えられない時
        iflag(1)=2
        iflag(2)=1
      endif
      cols=shape(self%xx) !行数、列数
      allocate(my_maxdim(cols(iflag(1)))) !列数分の配列
      !行方向の和->列毎に和を集計することになる
      my_maxdim=maxval(self%xx,dim=iflag(2)) 
      !最大値
      if(allocated(self%maxvalue))then
        deallocate(self%maxvalue)
      endif
      self%maxvalue=my_maxdim
    end subroutine
    !--------------------------------------------------------
    !>@brief 標準偏差standard deviationを求める
    !>
    !>@param[inout] self 計算したいデータ
    !>@param[in]  dir 1:列毎のstd、2:行毎のstd
    pure elemental subroutine calstd(self,dir)
      class(t_dfstats),intent(inout) :: self
      integer,intent(in),optional :: dir !1:列毎の平均、2:行毎の平均
      integer :: tmp_dir
      if(present(dir))then
        tmp_dir=dir
      else
        tmp_dir=1
      endif
      !標準偏差
      call self%calvar(dir=tmp_dir)
      if(allocated(self%std))then
        deallocate(self%std)
      endif
      allocate(self%std(size(self%variance)))
      self%std=sqrt(self%variance)
    end subroutine
    !--------------------------------------------------------
    !>@brief 不偏分散を求める
    !>
    !>@param[inout] self 計算したいデータ
    !>@param[in]  dir 1:列毎のvar、2:行毎のvar
    !>@param[out]   ier (optional)エラーフラグ。0なら正常。
    pure elemental subroutine calvar(self,dir,ier)
      class(t_dfstats),intent(inout) :: self
      integer,intent(in),optional :: dir !1:列毎の平均、2:行毎の平均
      integer,intent(out),optional :: ier !エラーフラグ
      integer,dimension(2) :: cols
      integer,dimension(2) :: iflag
      integer :: i
      if(present(ier))ier=0 !エラーがなければ0
      if(present(dir))then
        !dirが与えられた時
        if(dir==1)then
          !列方向の和
          iflag(1)=2 
          iflag(2)=1
        elseif(dir==2)then
          !行方向の和
          iflag(1)=1
          iflag(2)=2
        else
          !引数不正
          !write(*,*)"dir should be '1' or '2'."
          if(present(ier))ier=-1
          return
        endif
      else
        !dirが与えられない時
        iflag(1)=2
        iflag(2)=1
      endif
      cols=shape(self%xx) !行数、列数
      !現在の平均値が'行/列'のどちらの平均値かわからないので
      !平均値を計算し直す
      call self%calave(dir=iflag(2))
      if(allocated(self%variance))then
        deallocate(self%variance)
      endif
      allocate(self%variance(cols(iflag(1))))
      if(iflag(1)==2)then
        do i=1,cols(iflag(1))
          self%variance(i)= &
            sum((self%average(i)-self%xx(:,i))**2)/dble(cols(iflag(1))-1)
        enddo
      else
        do i=1,cols(iflag(1))
          self%variance(i)= &
            sum((self%average(i)-self%xx(i,:))**2)/dble(cols(iflag(1))-1)
        enddo
      endif
    end subroutine
    !--------------------------------------------------------
    !>@brief 列毎の平均値を求める
    !>
    !>@param[inout] self 計算したいデータ
    !>@param[in]  dir 1:列毎の平均、2:行毎の平均
    !>@param[out]   ier (optional)エラーフラグ。0なら正常。
    pure elemental subroutine calave(self,dir,ier) !result(average)
      class(t_dfstats),intent(inout) :: self
      integer,intent(in),optional :: dir !1:列毎の平均、2:行毎の平均
      integer,intent(out),optional :: ier !エラーフラグ
      real(kind=dp),dimension(:),allocatable :: average
      integer,dimension(2) :: cols
      integer,dimension(2) :: iflag
      if(present(ier))ier=0 !エラーがなければ0
      if(present(dir))then
        !dirが与えられた時
        if(dir==1)then
          !列方向の和
          iflag(1)=2 
          iflag(2)=1
        elseif(dir==2)then
          !行方向の和
          iflag(1)=1
          iflag(2)=2
        else
          !引数不正
          !write(*,*)"dir should be '1' or '2'."
          if(present(ier))ier=-1 !エラーがなければ0
          return
        endif
      else
        !dirが与えられない時
        iflag(1)=2
        iflag(2)=1
      endif
      cols=shape(self%xx) !行数、列数
      allocate(average(cols(iflag(1)))) !列数分の配列
      !行方向の和->列毎に和を集計することになる
      average=sum(self%xx,dim=iflag(2)) 
      average=average/dble(cols(iflag(2))) !平均値
      !自動reallocation機能で上書き
      self%average=average
    end subroutine
    !--------------------------------------------------------
    !>@brief データファイル読み込み関数(テキストサーチ機能を省略し高速動作)
    !>
    !>@param[in] df      読み込んだデータを含むt_dfstats型
    !>@param[in] fname   ファイル名
    !>@param[in] nrow    読み込むデータの行数。skipで飛ばした後、nrow行読み込む。
    !>@param[in] ncol    読み込むデータの列数。
    !>@param[in] skip    コメントでは無いが先頭を指定した行数分だけスキップ(optional,デフォルト=0)
    !>@param[in] sel     読み込む列を選択するための添字番号配列(optional)selの指定順にxxへ格納される
    subroutine dlmread_nosearch(df,fname,nrow,ncol,skip,sel) 
      class(t_dfstats),intent(out) :: df
      character(len=*),intent(in) :: fname
      integer,intent(in) :: nrow,ncol !読み込み行と列の数
      integer,optional :: skip !コメントラインでは無い行を読み飛ばす数
      integer,optional,dimension(:),intent(in) :: sel !読み込む列を選択するための添字番号配列
      integer :: skiphead,i,funit,astat
      character(len=1) :: txt
      real(kind=dp),dimension(ncol) :: tmp
      call destroy_t_dfstats(df)
      allocate(df%filename,source=fname)
      !引数の有無を確認
      if(.not.present(skip))then
        skiphead=0
      else
        skiphead=skip
      endif
      !message
      print*,"--- dlmread_nosearch -------"
      print*,"now reading data... "//trim(fname)
      print*,"先頭読み飛ばし指定(skip)によるスキップ=",skiphead
      print*,'(1X,3(A,I10,2X))',"データ行数=",nrow,"列数=",ncol
      !データの割付
      if(present(sel))then
        allocate(df%xx(nrow,size(sel)),stat=astat)
      else
        allocate(df%xx(nrow,ncol),stat=astat)
      endif
      if(astat.ne.0)then
        stop "type(t_dfstats)の要素を確保できませんでした．"
      endif
      !データの読み込み
      open(file=fname,newunit=funit,status='old')
      do i=1,skiphead
        read(funit,*) txt
      enddo
      do i=1,nrow
        read(funit,*) tmp(1:ncol)
        if(present(sel))then
          df%xx(i,1:size(sel))=tmp(sel) 
        else
          df%xx(i,1:ncol)=tmp(1:ncol) 
        endif
      enddo
      close(funit)
      !message
      print*,"データ読み込み終了."
      print*,"--- dlmread_nosearch end ---"
    end subroutine
    !--------------------------------------------------------
    !>@brief データファイル書き出し関数
    !>
    !>@param[in] df 書き出すデータ
    !>@param[in] fname 書き出すファイル名
    !>@param[in] sep   データ区切り
    subroutine dlmwrite(df,fname,sep)
      class(t_dfstats)  ,intent(in) :: df
      character(len=*),intent(in) :: fname,sep
      integer :: i,j,funit,ncol,nrow
      character(len=80) :: fmttxt,sep_txt
      !区切り文字が"タブ"のときは，アスキーコード表からタブを取得
      if(trim(sep)=="\t")then
        sep_txt=achar(9)
        !print*,"sep_txt=",sep_txt,"hoge",len_trim(sep_txt)
      else
        sep_txt=sep
      endif
      if(.not.allocated(df%xx))return !書き出し項目なし
      nrow=size(df%xx(:,1))
      ncol=size(df%xx(1,:))
      write(fmttxt,'(A,g0,A)') '(',nrow,'(g0,a))'
      open(file=fname,newunit=funit,status='replace')
      write(funit,'(A)') "# data from : "//trim(df%filename)
      do i=1,nrow
        do j=1,nrow-1
          write(funit,fmttxt,advance="no") df%xx(i,j),trim(sep_txt)
        enddo
        write(funit,fmttxt,advance="yes") df%xx(i,nrow)
      enddo
      close(funit)
    end subroutine
    !--------------------------------------------------------
    !>@brief データファイル読み込み関数
    !>
    !>通常のread関数より処理時間がかかるものの、octaveのdlmread関数の感覚で
    !>利用することができる。
    !>@param[in] df      読み込んだデータを含むt_dfstats型
    !>@param[in] fname   ファイル名
    !>@param[in] comment コメント文字
    !>@param[in] sep     列区切り文字列
    !>@param[in] skip    コメントでは無いが先頭を指定した行数分だけスキップ(optional,デフォルト=0)
    !>@param[in] sel     読み込む列を選択するための添字番号配列(optional)selの指定順にxxへ格納される
    subroutine dlmread(df,fname,comment,sep,skip,sel) !result(df)
      class(t_dfstats),intent(out) :: df
      character(len=*),intent(in) :: fname,comment,sep
      character(len=:),allocatable :: linebuf
      character(len=1) :: txt
      integer,optional :: skip !コメントラインでは無い行を読み飛ばす数
      integer,optional,dimension(:),intent(in) :: sel !読み込む列を選択するための添字番号配列
      integer :: i,allline,skipline,nline,ncol,skiphead,charsize,funit
      integer :: astat !allocationstatus
      integer,dimension(2) :: ict
      character (len=:),allocatable :: sep_txt !テキストファイルセパレーター

      call destroy_t_dfstats(df)
      allocate(df%filename,source=fname)
      allocate(sep_txt,source=sep)
      !区切り文字が"タブ"のときは，アスキーコード表からタブを取得
      if(trim(sep)=="\t")then
        sep_txt=achar(9)
        !print*,"sep_txt=",sep_txt,"hoge",len_trim(sep_txt)
      endif

      !先頭からのスキップ数を示す引数の有無を確認
      if(.not.present(skip))then
        skiphead=0
      else
        skiphead=skip
      endif

      !ファイルの1:行数,2:文字数を得る
      ict(1:2)=countalllines(fname)
      allline=ict(1)  !行数
      charsize=ict(2) !文字数
      !ラインバッファは最大charsizeで良い
      allocate(linebuf,source=repeat('x',charsize))

      !skiphead行以降で、コメントと空行によりスキップする数を数える
      skipline=countskiplines(fname,comment,skiphead)
      nline=allline-skipline-skiphead
      !open(10,file=fname,status='old')
      open(file=fname,newunit=funit,status='old')
        !コメントの有無に関係なく先頭行をスキップ
        do i=1,skiphead !skipline
          read(funit,'(A)') txt
        enddo
        !コメント行と空行を読み飛ばしてデータ列数を調べる
        do 
          read(funit,'(A)',end=999) linebuf
          if(comment_or_nullline(linebuf,comment))then
            exit
          endif
        enddo
        999 backspace(unit=funit)
        !ファイルシーケンスがデータ行の先頭にあるので列数を数える
        ncol=getcol(linebuf,sep_txt)
        if(present(sel))then
          allocate(df%xx(nline,size(sel)),stat=astat)
        else
          allocate(df%xx(nline,ncol),stat=astat)
        endif
        if(astat.ne.0)then
          stop "type(t_dfstats)の要素を確保できませんでした．"
        endif
        !message
        print*,"--- dlmread -------"
        print*,"now reading data... "//trim(fname)
        print*,"コメントと空行によるスキップ=",skipline
        print*,'(1X,3(3A,I3))',"区切り文字=",sep_txt,",ASCII文字コード番号=",ichar(sep_txt)
        print*,"先頭読み飛ばし指定(skip)によるスキップ=",skiphead
        print*,'(1X,3(A,I10,2X))',"全行数=",allline,"読み込み行数=",nline,"文字数=",charsize
        print*,"列数=",ncol
        print*,"--- dlmread end ---"
        !reading data
        i=1
        do 
          read(funit,'(A)') linebuf
          if(comment_or_nullline(linebuf,comment))then
            !コメントでは無い行に関する操作
            call df%getnum(trim(linebuf),sep_txt,i,ncol,sel)
            i=i+1
          endif
          if(i==nline+1)exit
        enddo
       !do i=1,nline
       !  read(10,'(A)') linebuf
       !  call df%getnum(trim(linebuf),sep_txt,i,ncol)
       !enddo
        print*,"reading data : DONE"
      close(funit)
    end subroutine
    !---------------------------------------------------
    !>@brief 読み込まれた1行(テキスト)を数値に変換する
    subroutine getnum(self,txt,sep,lnum,ncol,sel) 
      class(t_dfstats),intent(inout) :: self
      integer,intent(in) :: ncol,lnum
      character (len=*),intent(in)  :: txt,sep
      character (len=:),allocatable :: txt_trim
      character (len=30) :: txtmp
      integer :: slen,i,j,indx
      integer,optional,dimension(:),intent(in) :: sel !読み込む列を選択するための添字番号配列
      !引数を変更しないようにする
      allocate(txt_trim,source=adjustl(txt))
      slen=len_trim(txt_trim)
      !列数を数える
      do j=1,ncol
        i=index(txt_trim(1:slen),sep) !sepの位置
        !txttmpは、取り出したい文字列
        if(i==0.and.slen>0)then
          txtmp=txt_trim(1:slen)
        else
          txtmp=txt_trim(1:i-1)
        endif
        if(present(sel))then
          if(any(j-sel==0))then
            indx=minloc(abs(sel-j),dim=1)
            read(txtmp,*)self%xx(lnum,indx)
          endif
        else
          read(txtmp,*)self%xx(lnum,j)
        endif
        txt_trim=trim(adjustl(txt_trim(i+1:slen)))
        slen=len_trim(txt_trim)
        if(i==0) exit
      enddo
      deallocate(txt_trim)
    end subroutine
    !---------------------------------------------------
    !>@brief 行数と文字列数の最大値を数える
    function countalllines(fname) result(ict)
      character(len=*),intent(in) :: fname
      character(len=1) :: linebuf
      integer :: nline,charsize,istat,funit
      integer,dimension(2) :: ict !行数、文字数
      ict=0
      nline=0
      charsize=0
      open(file=fname,newunit=funit,status='old')
        do 
          read(funit,'(A)',advance="no",iostat=istat) linebuf
          ict(2)=ict(2)+1 !１行中の文字数カウント
          if(istat==iostat_end)exit !ファイルの終端ならdo抜ける
          if(istat==iostat_eor)then
            !行の終端に達した
            nline=nline+1
            charsize=max(charsize,ict(2))
            ict(2)=0
          endif
        end do
      close(funit)
      ict=(/nline,charsize/)
    end function
    !---------------------------------------------------
    !>@brief コメント行と空行の数を数える関数。
    !>
    !>@param[in] fname ファイル名
    !>@param[in] comment コメント文字
    !>@param[in] skiphead 先頭からのスキップ行数
    !>@retval nskip skiphead以降に含まれるコメント行と空行の合計
    function countskiplines(fname,comment,skiphead) result(nskip)
      integer :: ict,nskip,funit,istat
      integer,intent(in) :: skiphead
      character(len=*),intent(in) :: comment,fname
      character(len=100) :: line
      open(newunit=funit,file=fname,status='old')
        !先頭を読み飛ばす場合
        do ict=1,skiphead
          read(funit,'(A)',iostat=istat) line
        enddo
        !skiphead以降で、コメント行や空行を数える
        nskip=0
        do 
          read(funit,'(A)',iostat=istat) line
          if(istat==iostat_end)exit !ファイルの終端ならdo抜ける
          !コメント、または空行のとき
          if(.not.comment_or_nullline(line,comment))then
            nskip=nskip+1
          endif
        enddo
      close(funit)
    end function
    !---------------------------------------------------
    !>@brief 一行分のデータを取得して、sepで区切られた個数を返す関数。
    !>
    !>@param[in] txt 一行分のテキスト
    !>@param[in] sep 区切り文字
    !>@retval 区切り文字で仕切られた個数(列数)
    function getcol(txt,sep) result(ncol)
      integer :: slen,i,ncol
      character (len=*),intent(in)  :: txt,sep
      character (len=:),allocatable :: txt_trim
      !引数を変更しないようにする
      allocate(txt_trim,source=adjustl(txt))
      slen=len_trim(txt_trim)
      !列数を数える
      ncol=0
      do 
        i=index(txt_trim(1:slen),sep) !sepの位置
        ncol=ncol+1
        txt_trim=trim(adjustl(txt_trim(i+1:slen)))
        slen=len_trim(txt_trim)
        if(i==0) exit
      enddo
      deallocate(txt_trim)
    end function
    !---------------------------------------------------
    !>@brief 読み込まれた行がコメント行または空行でない場合に真を返す
    !>
    !>@param[in] linebuf 読み込んだ行のテキストデータ
    !>@param[in] comment コメント文字
    !>@retval tf コメント行または空行でない場合に真
    function comment_or_nullline(linebuf,comment) result(tf)
      character(len=*),intent(in) :: linebuf,comment
      character(len=:),allocatable :: line
      logical :: tf
      allocate(line,source=trim(adjustl(linebuf))) !左寄せ
      tf=scan(line,comment)==0.and.len(line).ne.0
    end function
end module
