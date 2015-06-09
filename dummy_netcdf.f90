program dummy_netcdf

  use kinds, only : r_single
  use read_dummy, only: header_list, data_list, get_header, get_data, mean_type, &
                        stddev_type, inc_mean, ret_mean, ret_mean_from_array, init_dummy, &
                        set_ndata, print_array, read_array, write_array, &
!                        read_array_hdr, read_array_data, 
                        array_plus1,    &
                        new_type_derived, allocate_bifix_inside, set_bifix_dimension, &
                        allocate_obstype_arr_inside, set_obstype_arr_dimension, set_array_data_real,  &
                        set_array_data_char10, print_array_char10, logical_subroutine, allocate_new_type
  use netcdf_layer, only: init_netcdf

  type(header_list)      :: head
  type(data_list)        :: dat
  type(new_type_derived),dimension(:),allocatable :: new_inside, new_outside
  integer                                         :: newtype_dim = 2

  type(mean_type)     :: cur_mean
  real(r_single),dimension(:),allocatable  :: cur_array, array_from_file

  character(len=120) :: fn

  integer i, ifn, lun, ndata_from_file

  integer                :: generic_dim = 5

  call init_dummy ! initialize the module

  call set_ndata(15)  !just for the heck of it, increase ndata to 15

  call get_header(ifn,head) ! First, we 'read' the header 

  write(*,fmt='(A25,1X,A20)')'head%header_title=',head%header_title
  write(*,fmt='(A25,1X,I10)')'head%ndata=',head%ndata
  write(*,fmt='(A25,1X,I10)')'head%nchan=',head%nchan
  write(*,fmt='(A25,1X,F10.3)')'head%dummy=',head%dummy

  allocate(cur_array(head%ndata))

  do i=1,head%ndata          ! then we loop over the ndata from the header
    call get_data(ifn,dat)   !   and read each data point in a loop
    write(*,fmt='(A25,1X,F10.3)')'dat%obs=',dat%obs
    write(*,fmt='(A25,1X,F10.3)')'dat%error=',dat%error
    call inc_mean(dat%obs, cur_mean) ! sequentially calculate mean
    cur_array(i) = dat%obs
  enddo

  call print_array(cur_array,head%ndata)

  print *,'Mean from mean_type:           ',ret_mean(cur_mean) ! return mean from sequential calculation
  print *,'Mean from ret_mean_from_array: ',ret_mean_from_array(cur_array,head%ndata) !return mean from instantaneous array calc

  lun = 123
  fn  = 'test.output.binary'

  call write_array(lun,fn,cur_array,head%ndata)            ! write array to 'self-describing' binary file (first element ndata, second element array(ndata) )
  call read_array(lun,fn,array_from_file,ndata_from_array) ! read array from 'self-describing' binary file

  call print_array(array_from_file,ndata_from_array)

!  deallocate(array_from_file)
!  call read_array_hdr(lun,fn,ndata_from_array) ! read array from 'self-describing' binary file

!  allocate(array_from_file(ndata_from_array))
!  call read_array_data(lun,fn,array_from_file,ndata_from_array) ! read array from 'self-describing' binary file

!  call print_array(array_from_file,ndata_from_array)

  print *,'Putting array_from_file through array_plus1'
  call array_plus1(array_from_file)
  call print_array(array_from_file,ndata_from_array)

  print *,'Allocating new_inside'
  call allocate_new_type(new_inside,newtype_dim)

  print *,'Allocating new_outside'
  allocate(new_outside(newtype_dim))


  do i=1,newtype_dim
    print *,'--------------------------------------------------------------------'
    print *,'DOING new_inside(i) & new_outside(i) for i=',i
    print *,'--------------------------------------------------------------------'

    print *,'Setting dimension for bifix allocation'
    call set_bifix_dimension(new_inside(i), generic_dim)
    call set_bifix_dimension(new_outside(i), generic_dim)

    print *,'Allocating bifix inside read_dummy'
    call allocate_bifix_inside(new_inside(i))

    print *,'Allocating bifix outside read_dummy'
    allocate(new_outside(i)%bifix(new_outside(i)%bifix_dim))

    print *,'Putting data in bifix'
    call set_array_data_real(new_inside(i)%bifix,new_inside(i)%bifix_dim)
    call set_array_data_real(new_outside(i)%bifix,new_outside(i)%bifix_dim)

    print *,'Printing new_inside(i)%bifix'
    call print_array(new_inside(i)%bifix,new_inside(i)%bifix_dim)
  
    print *,'Printing new_outside(i)%bifix'
    call print_array(new_outside(i)%bifix,new_outside(i)%bifix_dim)
  
    print *,'Setting dimension for obstype_arr allocation'
    call set_obstype_arr_dimension(new_inside(i), generic_dim)
    call set_obstype_arr_dimension(new_outside(i), generic_dim)
  
    print *,'Allocating obstype_arr inside read_dummy'
    call allocate_obstype_arr_inside(new_inside(i))
  
    print *,'Allocating obstype_arr outside read_dummy'
    allocate(new_outside(i)%obstype_arr(new_outside(i)%obstype_arr_dim))

    print *,'Putting data in obstype_arr'
    call set_array_data_char10(new_inside(i)%obstype_arr,new_inside(i)%obstype_arr_dim)
    call set_array_data_char10(new_outside(i)%obstype_arr,new_outside(i)%obstype_arr_dim)
  
    print *,'Printing new_inside(i)%obstype_arr'
    call print_array_char10(new_inside(i)%obstype_arr,new_inside(i)%obstype_arr_dim)

    print *,'Printing new_outside(i)%obstype_arr'
    call print_array_char10(new_outside(i)%obstype_arr,new_outside(i)%obstype_arr_dim)

    print *,'Sending .true. to logical_subroutine'
    call logical_subroutine(.true.)

    print *,'Sending .false. to logical_subroutine'
    call logical_subroutine(.false.)
  enddo


end program test_dummy

