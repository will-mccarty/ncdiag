module read_dummy

  use kinds, only:  i_kind,r_single,r_quad
  implicit none

! Declare public and private
  private

  public :: header_list
  public :: data_list
  public :: mean_type
  public :: stddev_type

  public :: init_dummy
  public :: set_ndata

  public :: get_header
  public :: get_data

  public :: inc_mean
  public :: ret_mean
  public :: ret_mean_from_array

  public :: print_array
  public :: write_array
  public :: read_array
!  public :: read_array_hdr
!  public :: read_array_data

  public :: print_array_char10

  public :: logical_subroutine

  public :: new_type_derived
  public :: allocate_new_type
  public :: allocate_bifix_inside
  public :: set_bifix_dimension
  public :: allocate_obstype_arr_inside
  public :: set_obstype_arr_dimension

  public :: set_array_data_real
  public :: set_array_data_char10

  public :: array_plus1	

  integer(i_kind)     :: data_ct = 1 ! internal counter, will increase by one every time get_data is called

  integer(i_kind)     :: ndata   = -999 

  logical             :: linit   = .false.

  type new_type_derived
    character(len=10)                          :: obstype
    character(len=10),dimension(:),allocatable :: obstype_arr
    integer(i_kind)                            :: obstype_arr_dim
    real(r_single),dimension(:),allocatable    :: bifix
    integer(i_kind)                            :: bifix_dim
  end type new_type_derived

  type header_list                    !  This is the structure type, containing:
    character(len=20) :: header_title    ! header title/description
    integer(i_kind)   :: ndata           ! number of data
    integer(i_kind)   :: nchan           ! number of channels - not used yet
    real(r_single)    :: dummy           ! just a dummy number
  end type header_list

  type data_list                      !  This is the data structure, containing:
    real(r_single)    :: obs             ! a fake observation (increases by 1 every time it is called)
    real(r_single)    :: error           ! a fake error (just -1 * obs)
  end type data_list

  type mean_type                      !  This is a structure to calculate a running mean
    real(r_quad)    :: total           ! mean = total / n
    real(r_quad)    :: n               !  
  end type mean_type

  type stddev_type                    !  This is a structure for a running standard deviation (Welford)
    real(r_quad)      :: n, delta, mean, M2   ! 
  end type stddev_type

contains
  subroutine init_dummy
    print *,'Initializing read_dummy module...default ndata = 5'
    ndata = 5
    linit = .true.
  end subroutine init_dummy

  subroutine set_ndata(nd)
    integer, intent(in) :: nd

    call test_init
    print *,'Setting ndata = ',nd
    ndata = nd
  end subroutine set_ndata

  subroutine test_init
    if (.not. linit) then
      print *,'ERROR: module not initialized - must call init_dummy'
      call abort
    endif
  end subroutine test_init

  subroutine inc_mean(ob, mean)
    real(r_single),intent(in)     :: ob
    type(mean_type),intent(inout) :: mean

    mean%total = mean%total + ob
    mean%n     = mean%n + 1
  end subroutine inc_mean 

  real(r_single) function ret_mean(mean)
    type(mean_type),intent(in)  :: mean

    ret_mean = mean%total / mean%n
  end function ret_mean

  subroutine set_bifix_dimension(newtype,dimen)
    type(new_type_derived),intent(inout) :: newtype
    integer(i_kind),intent(in)           :: dimen

    newtype%bifix_dim = dimen    
  end subroutine set_bifix_dimension

  subroutine allocate_new_type(newtype,dimen)
    type(new_type_derived),allocatable,intent(out) :: newtype(:)
    integer,intent(in)                             :: dimen

    allocate(newtype(dimen))
    print *,'Newtype Allocated'
  end subroutine allocate_new_type

  subroutine allocate_bifix_inside(newtype)
    type(new_type_derived),intent(inout) :: newtype

    allocate( newtype%bifix(newtype%bifix_dim))  
  end subroutine allocate_bifix_inside

  subroutine set_obstype_arr_dimension(newtype,dimen)
    type(new_type_derived),intent(inout) :: newtype
    integer(i_kind),intent(in)           :: dimen

    newtype%obstype_arr_dim = dimen
  end subroutine set_obstype_arr_dimension

  subroutine allocate_obstype_arr_inside(newtype)
    type(new_type_derived),intent(inout) :: newtype

    allocate( newtype%obstype_arr(newtype%obstype_arr_dim))
  end subroutine allocate_obstype_arr_inside

  subroutine set_array_data_real(array, dimen)
    real(r_single),intent(inout),dimension(*)   :: array
    integer(i_kind),intent(in)                      :: dimen

    integer i

    do i=1,dimen
      array(i) = i
    enddo    
  end subroutine set_array_data_real

  subroutine set_array_data_char10(array, dimen)
    character(len=10),intent(inout),dimension(*)   :: array
    integer(i_kind),intent(in)                      :: dimen

    integer i

    do i=1,dimen
      write(array(i),fmt='(A1,1X,A3,I5)') 'X','array',i
    enddo
  end subroutine set_array_data_char10

  subroutine logical_subroutine(lvariable)
    logical,intent(in) :: lvariable

    if (lvariable) then
      print *,'LOGICAL_SUBROUTINE:  input was true'
    else
      print *,'LOGICAL_SUBROUTINE:  input was false'
    endif
  end subroutine logical_subroutine

  real(r_single) function ret_mean_from_array(array, ndata)
    integer(i_kind),intent(in)              :: ndata
    real(r_single),dimension(*),intent(in)  :: array

    real(r_quad)    :: total, n
    integer(i_kind) :: i
  
    total = 0
    n = 0 

    do i=1,ndata
      total = total + array(i)
      n = n + 1
    enddo

    ret_mean_from_array = total / n
  end function ret_mean_from_array

  subroutine print_array(array, ndata)
    integer(i_kind),intent(in)              :: ndata
    real(r_single),dimension(*),intent(in)  :: array

    integer(i_kind) :: i

    do i=1,ndata
      print *,'i,array(i) = ',i,array(i)
    enddo
  end subroutine print_array

  subroutine print_array_char10(array, ndata)
    integer(i_kind),intent(in)              :: ndata
    character(len=10),dimension(*),intent(in)  :: array

    integer(i_kind) :: i

    do i=1,ndata
      print *,'i=',i,'array(i)=  ',array(i)
    enddo
  end subroutine print_array_char10


  subroutine array_plus1(array)
    real(r_single),dimension(:),intent(inout)  :: array

    array(:) = array(:) + 1
  end subroutine array_plus1


  subroutine write_array(lun,filename,array,ndata)
    integer(i_kind), intent(in)                           :: lun
    character(len=120),intent(in)                              :: filename
    real(r_single),dimension(ndata),intent(in) :: array
    integer(i_kind),intent(in)                           :: ndata 
    
    open(lun,file=filename,form='unformatted')
    print *,'Writing to filename:',filename
    print *,'Writing array of length=',ndata
    write(lun) ndata
    write(lun) array
    close(lun)
  end subroutine write_array

  subroutine read_array(lun,filename,array,ndata)
    integer(i_kind), intent(in)                           :: lun
    character(len=120),intent(in)                              :: filename
    real(r_single),dimension(:),intent(inout),allocatable :: array
    integer(i_kind), intent(out)                        :: ndata

    open(lun,file=filename,form='unformatted')
    print *,'Reading from filename:',filename
    read(lun) ndata
    print *,'Reading array of length=',ndata
    allocate(array(ndata))

    read(lun) array
    close(lun)
  end subroutine read_array

!  subroutine read_array_hdr(lun,filename,ndata)  bind(c)
!    integer(i_kind), intent(in)                           :: lun
!    character(len=120),intent(in)                              :: filename
!    integer(i_kind), intent(out)                        :: ndata
!
!    real(r_single),dimension(:),allocatable :: local_array
!
!    ndata = 0
!
!    call read_array(lun,filename,local_array,ndata)
!  end subroutine read_array_hdr

!  subroutine read_array_data(lun,filename,array,ndata) bind(c)
!    integer(i_kind), intent(in)                           :: lun
!    character(len=120),intent(in)                              :: filename
!    real(r_single),dimension(ndata),intent(inout)         :: array
!    integer(i_kind), intent(in)                           :: ndata
!
!    real(r_single),dimension(:),allocatable :: local_array
!    integer(i_kind)                         :: local_ndata
!    call read_array(lun,filename,local_array,local_ndata)
!    if (local_ndata .eq. ndata) then
!      array = local_array
!    else
!      print *,'Yo your ndata did not match the data in the file'
!      call abort
!    endif
!
!  end subroutine read_array_data



  subroutine get_header(inlun,head)
    integer(i_kind),intent(in)    :: inlun
    type(header_list),intent(out) :: head

    call test_init

    write(*,*)'Getting Header...'

    head%header_title = 'Hi Albert!'
    head%ndata        = ndata
    head%nchan        = 1
    head%dummy        = 1.2345
  end subroutine get_header

  subroutine get_data(inlun,dat)
    integer(i_kind),intent(in)    :: inlun
    type(data_list),intent(out) :: dat

    write(*,*)'Getting Data...'

    dat%obs   = data_ct
    dat%error = -1.0 * data_ct
    data_ct = data_ct + 1
  end subroutine get_data

end module read_dummy
  

    
