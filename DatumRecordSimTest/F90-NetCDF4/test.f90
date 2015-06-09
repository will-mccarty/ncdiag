module TestDatumRecordVars
    integer, parameter :: DIM_X = 10
    integer, parameter :: DIM_Y = 10
    integer, parameter :: ITERS = 100000000
end module TestDatumRecordVars

program TestDatumRecord
    use netcdf
    use TestDatumRecordVars
    implicit none
    
    integer :: counter
    integer :: tmp
    integer :: i, j, k
    
    integer :: t1, t2
    real    :: rate
    integer :: cr, cm
    
    real, dimension(DIM_X, DIM_Y)  :: record
    real, dimension(DIM_X * DIM_Y) :: datum
    
    ! =================================================================
    ! NetCDF definitions
    ! =================================================================
    
    character (len = *), parameter :: FILE_NAME_RECORD = "test_record.nc"
    character (len = *), parameter :: FILE_NAME_DATUM  = "test_datum.nc"
    integer :: ncid_record, ncid_datum
    
    ! We are reading 2D data, a 6 x 12 lat-lon grid.
    integer, parameter :: NDIMS = 2
    integer, parameter :: NDIMX = 6, NDIMY = 12
    character (len = *), parameter :: COL1_NAME  = "Person_ID"
    character (len = *), parameter :: COL2_NAME  = "Cookies"
    character (len = *), parameter :: COL3_NAME  = "Cupcakes"
    character (len = *), parameter :: COL4_NAME  = "Muffins"
    character (len = *), parameter :: COL5_NAME  = "Bagels"
    character (len = *), parameter :: COL6_NAME  = "Donuts"
    character (len = *), parameter :: COL7_NAME  = "Cakes"
    character (len = *), parameter :: COL8_NAME  = "Loaves"
    character (len = *), parameter :: COL9_NAME  = "Rolls"
    character (len = *), parameter :: COL10_NAME = "Fried_Chicken"
    integer :: lat_dimid, lon_dimid
    
    ! Initialize the system_clock
    CALL system_clock(count_rate=cr)
    CALL system_clock(count_max=cm)
    
    ! Initialize data
    call initRecord(record)
    call initDatum(datum)
    
    ! Validate record format
    call system_clock (t1)
    
    do k = 1, ITERS, 1
        counter = 0
        tmp = 0
        do i = 1, DIM_X, 1
            do j = 1, DIM_Y, 1
                tmp = getRecord(record, i, j)
                if (tmp /= counter) then
                    write(*, "(A, I3, A, I3, A, I3, A, I3)") "[RECORD] ERROR: at i =", i, " and j =", j, ", result tmp =", tmp, " does not match correct result", counter
                endif
                counter = counter + 1
            enddo
        enddo
    enddo
    call system_clock (t2)
    write ( *, * ) '[RECORD] Elapsed real time = ', real ( t2 - t1 ) / real ( cr )
    
    ! Validate datum format
    call system_clock (t1)
    
    do k = 1, ITERS, 1
        counter = 0
        tmp = 0
        do j = 1, DIM_Y, 1
            do i = 1, DIM_X, 1
                tmp = getDatum(datum, i, j)
                if (tmp /= counter) then
                    write(*, "(A, I3, A, I3, A, I3, A, I3)") "[DATUM]  ERROR: at i =", i, " and j =", j, ", result tmp =", tmp, " does not match correct result", counter
                    ! print *, "[RECORD] ERROR: at i ="
                    ! write(*, "(I3)") i
                    ! print *, " and j = "
                    ! write(*, "(I3)") j
                    ! print *, ", result tmp = "
                    ! write(*, "(I3)") tmp
                    ! print *, "does not match correct result"
                    ! write(*, "(I3)") counter
                endif
                counter = counter + 1
            enddo
        enddo
    enddo
    call system_clock (t2)
    write ( *, * ) '[DATUM]  Elapsed real time = ', real ( t2 - t1 ) / real ( cr )
    
    contains
        subroutine initRecord(record)
            use TestDatumRecordVars
            real, dimension(DIM_X, DIM_Y), intent(inout) :: record
            integer :: counter
            
            counter = 0
            do i = 1, DIM_X, 1
                do j = 1, DIM_Y, 1
                    record(i, j) = counter
                    counter = counter + 1
                enddo
            enddo
        end subroutine initRecord
        
        subroutine initDatum(datum)
            use TestDatumRecordVars
            real, dimension(DIM_X * DIM_Y), intent(inout) :: datum
            integer :: counter
            
            counter = 0
            do i = 1, DIM_X * DIM_Y, 1
                datum(i) = i - 1
            enddo
        end subroutine initDatum
        
        real function getRecord(record_in, x, y)
            use TestDatumRecordVars
            real, dimension(DIM_X, DIM_Y), intent(in) :: record_in
            integer, intent(in)                       :: x
            integer, intent(in)                       :: y
            
            getRecord = record(x, y)
        end function getRecord
        
        real function getDatum(datum_in, x, y)
            use TestDatumRecordVars
            real, dimension(DIM_X * DIM_Y), intent(in) :: datum_in
            integer, intent(in)                       :: x
            integer, intent(in)                       :: y
            
            getDatum = datum(DIM_X * (y - 1) + x)
        end function getDatum
end program TestDatumRecord
