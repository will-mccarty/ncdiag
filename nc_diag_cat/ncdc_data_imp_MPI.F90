        subroutine nc_diag_cat_data_pass
            integer :: cur_dim_id, cur_dim_len
            integer :: cur_out_var_id, cur_out_var_ndims, cur_out_var_counter
            integer :: cur_out_dim_ind, cur_out_var_ind, cur_out_var_type
            integer, dimension(:), allocatable :: cur_out_dim_ids, cur_dim_ids
            integer, dimension(:), allocatable :: cur_out_dim_sizes
            integer, dimension(:), allocatable :: cur_dim_sizes
            
            integer(i_byte),    dimension(:), allocatable     :: byte_buffer
            integer(i_short),   dimension(:), allocatable     :: short_buffer
            integer(i_long),    dimension(:), allocatable     :: long_buffer
            
            real(r_single),     dimension(:), allocatable     :: rsingle_buffer
            real(r_double),     dimension(:), allocatable     :: rdouble_buffer
            
            !character(len=1000),dimension(:), allocatable     :: string_buffer
            character(1)     ,dimension(:,:), allocatable     :: string_buffer
            character(1)     ,dimension(:,:), allocatable     :: string_expanded_buffer
            character(1)     ,dimension(:),   allocatable     :: string_1d_buffer
            
            integer(i_byte),  dimension(:,:), allocatable     :: byte_2d_buffer
            integer(i_short), dimension(:,:), allocatable     :: short_2d_buffer
            integer(i_long),  dimension(:,:), allocatable     :: long_2d_buffer
            
            real(r_single),   dimension(:,:), allocatable     :: rsingle_2d_buffer
            real(r_double),   dimension(:,:), allocatable     :: rdouble_2d_buffer
            
            character(1),   dimension(:,:,:), allocatable     :: string_2d_buffer
            character(1),   dimension(:,:,:), allocatable     :: string_2d_expanded_buffer
            
            type temp_storage
                integer(i_byte),    dimension(:), allocatable     :: byte_buffer
                integer(i_short),   dimension(:), allocatable     :: short_buffer
                integer(i_long),    dimension(:), allocatable     :: long_buffer
                
                real(r_single),     dimension(:), allocatable     :: rsingle_buffer
                real(r_double),     dimension(:), allocatable     :: rdouble_buffer
                
                !character(len=1000),dimension(:), allocatable     :: string_buffer
                character(1)     ,dimension(:,:), allocatable     :: string_buffer
                character(1)     ,dimension(:,:), allocatable     :: string_expanded_buffer
                character(1)     ,dimension(:),   allocatable     :: string_1d_buffer
                
                integer(i_byte),  dimension(:,:), allocatable     :: byte_2d_buffer
                integer(i_short), dimension(:,:), allocatable     :: short_2d_buffer
                integer(i_long),  dimension(:,:), allocatable     :: long_2d_buffer
                
                real(r_single),   dimension(:,:), allocatable     :: rsingle_2d_buffer
                real(r_double),   dimension(:,:), allocatable     :: rdouble_2d_buffer
                
                character(1),   dimension(:,:,:), allocatable     :: string_2d_buffer
                character(1),   dimension(:,:,:), allocatable     :: string_2d_expanded_buffer
            end type temp_storage
            
            type(temp_storage), dimension(:), allocatable         :: temp_storage_arr
            
            character(1) ,  dimension(:), allocatable         :: tmp_str_buffer
            
            integer :: i, j, i_proc, procs_done = 0, base_proc = 1
            integer :: num_count, read_count = 0, file_count = 0
            
            integer(i_long),  dimension(:),   allocatable     :: procs_done_arr
            
            logical :: mpi_read_flag = .FALSE.
            
            integer(i_long),    dimension(:), allocatable     :: read_var_count
            
            integer(i_long) :: mpi_status(MPI_STATUS_SIZE)
            integer(i_long),    dimension(:), allocatable     :: mpi_requests
            integer(i_long)                                   :: mpi_requests_total = 0
            
            integer(i_long)                                   :: mpi_request_EOF
            integer(i_long)                                   :: mpi_request_EOP
            
            character(len=NF90_MAX_NAME) , allocatable :: tmp_in_dim_names(:)
            
            character(len=1000)                :: err_string
            
            character(:), allocatable :: input_file_cut
            
            if (.NOT. allocated(var_names)) then
                call warning("No variables found to concatenate.")
                return
            end if
            
#ifdef DEBUG
            print *, " !!! BEGINNING DATA PASS!!"
#endif
            
            input_count = cli_arg_count - 2
            
            if (cur_proc /= 0) then
                call info("Reading in data from all files...")
                
                ! Allocate the correct amount of requests needed for the
                ! files and variables!
                ! 
                ! We need (num of files * num of vars) space.
                ! 
                ! Number of files is a bit tricky to determine, but not too bad!
                !  -> If the total number of files divides evenly into the number
                !     of processors handling files (num_procs - 1), then we just
                !     divide and multiply.
                !  -> If we have a remainder, and the current process is less
                !     than or equal to (input_count % (num_procs - 1)), do the
                !     same, but add 1 extra after dividing, THEN multiply.
                !  -> If we have a remainder, but we are greater than that,
                !     just do simple division and multiplication without adding
                !     anything.
                
                if (mod(input_count, num_procs - 1) == 0) then
                    allocate(mpi_requests((input_count / (num_procs - 1)) * (var_arr_total + 1) + 1))
                    allocate(temp_storage_arr((input_count / (num_procs - 1)) * (var_arr_total + 1) + 1))
                else
                    if (cur_proc <= mod(input_count, num_procs - 1)) then
                        allocate(mpi_requests(((input_count / (num_procs - 1)) + 1) * (var_arr_total + 1) + 1))
                        allocate(temp_storage_arr(((input_count / (num_procs - 1)) + 1) * (var_arr_total + 1) + 1))
                    else
                        allocate(mpi_requests((input_count / (num_procs - 1)) * (var_arr_total + 1) + 1))
                        allocate(temp_storage_arr((input_count / (num_procs - 1)) * (var_arr_total + 1) + 1))
                    end if
                end if
                
                mpi_request_EOF = var_arr_total + 1000
                mpi_request_EOP = var_arr_total + 2000
                
                ! For each processor 1 ... n, do every (n - proc + 1) task.
                ! Example:
                !     Total # of tasks:      20
                !     Total # of processors: 5 (so 0 root, 1-4)
                !     Formula: (task # - 1) mod (# procs - 1) == (proc # - 1)
                !                                            Zero Indexed     | Fortran Indexed
                !     processor 0: is collecting flowers and not doing anything
                !     processor 1: (task # - 1) mod 4 == 0 | 0, 4, 8,  12, 16 | 1, 5, 9,  13, 17
                !     processor 2: (task # - 1) mod 4 == 1 | 1, 5, 9,  13, 17 | 2, 6, 10, 14, 18
                !     processor 3: (task # - 1) mod 4 == 2 | 2, 6, 10, 14, 18 | 3, 7, 11, 15, 19
                !     processor 4: (task # - 1) mod 4 == 3 | 3, 7, 11, 15, 19 | 4, 8, 12, 16, 20
                ! We could do an if statement using mod... but can we do better? YES!
                ! Looking at the Fortran indexed tasks for each processor, we can set
                ! our initial to (proc #), and then just add (num_procs - 1) after.
                !print *, "start index = cur_proc = ", cur_proc
                !print *, "end   index = input_count = ", input_count
                !print *, "interval = num_procs - 1 = ", num_procs - 1
                do arg_index = cur_proc, input_count, num_procs - 1
                    !write (info_str, "(A, I0)") "arg_index = ", arg_index
                    !call info(trim(info_str))
                    
                    call get_command_argument(2 + arg_index, input_file)
                    
                    input_file_cut = trim(input_file)
                    
                    if (len(input_file_cut) <= 0) then
                        call usage("Invalid input file name - likely blank!")
                    end if
                    
                    if (input_file_cut == output_file) then
                        ! No warning here - we've already shown it in metadata.
                        call info(" -> Skipping " // input_file_cut // " since it is the output file...")
                    else
                        call info(" -> Reading data from " // input_file_cut // "...")
                        call check(nf90_open(input_file, NF90_NOWRITE, ncid_input, &
                            cache_size = 2147483647))
                        
                        ! Get top level info about the file!
                        call check(nf90_inquire(ncid_input, nDimensions = input_ndims, &
                            nVariables = input_nvars, nAttributes = input_nattrs))
                        
                        ! Dimensions
                        allocate(tmp_in_dim_names(input_ndims))
                        do tmp_dim_index = 1, input_ndims
                            call check(nf90_inquire_dimension(ncid_input, tmp_dim_index, &
                                tmp_in_dim_names(tmp_dim_index)))
                        end do
                        
                        ! Variables
                        allocate(tmp_input_varids(input_nvars))
                        
                        ! Loop through each variable!
                        do var_index = 1, input_nvars
                            ! Grab number of dimensions and attributes first
                            call check(nf90_inquire_variable(ncid_input, var_index, name = tmp_var_name, ndims = tmp_var_ndims))
                            
                            ! Allocate temporary variable dimids storage!
                            allocate(tmp_var_dimids(tmp_var_ndims))
                            allocate(tmp_var_dim_names(tmp_var_ndims))
                            allocate(cur_dim_ids(tmp_var_ndims))
                            allocate(cur_dim_sizes(tmp_var_ndims))
                            allocate(cur_out_dim_ids(tmp_var_ndims))
                            allocate(cur_out_dim_sizes(tmp_var_ndims))
                            
                            ! Grab the actual dimension IDs and attributes
                            call check(nf90_inquire_variable(ncid_input, var_index, dimids = tmp_var_dimids, &
                                xtype = tmp_var_type))
                            
                            do i = 1, tmp_var_ndims
                                call check(nf90_inquire_dimension(ncid_input, tmp_var_dimids(i), tmp_var_dim_names(i), cur_dim_sizes(i)))
                                cur_out_dim_ind = nc_diag_cat_lookup_dim(tmp_var_dim_names(i))
                                cur_out_dim_ids(i)   = dim_output_ids(cur_out_dim_ind)
                                cur_out_dim_sizes(i) = dim_sizes(cur_out_dim_ind)
                            end do
                            
                            ! Now, let's lookup everything and translate the result to our file.
                            cur_out_var_ind = nc_diag_cat_lookup_var(tmp_var_name)
                            cur_out_var_id = var_output_ids(cur_out_var_ind)
                            cur_out_var_ndims = var_dim_names(cur_out_var_ind)%num_names
                            cur_out_var_counter = var_counters(cur_out_var_ind)
                            
                            !print *, "VARIABLE: " // trim(var_names(cur_out_var_ind))
                            !print *, "ALLOC SIZES:", data_blobs(cur_out_var_ind)%alloc_size
                            !print *, "CUR POS:", data_blobs(cur_out_var_ind)%cur_pos
                            !print *, "DIM SIZES:", cur_out_dim_sizes
                            !print *, "VAR COUNTER:", cur_out_var_counter
                            
                            ! Check for one-time only vars...
                            if (((.NOT. any(cur_out_dim_sizes == -1)) .AND. (cur_out_var_counter == 0)) &
                                .OR. (any(cur_out_dim_sizes == -1))) then
                                !call info("VARIABLE: " // trim(var_names(cur_out_var_ind)))
                                !write (info_str, "(A, I0)") "VAR COUNTER: ", cur_out_var_counter
                                !call info(trim(info_str))
                                if ((cur_out_var_ndims == 1) .OR. &
                                    ((cur_out_var_ndims == 2) .AND. (tmp_var_type == NF90_CHAR))) then
                                    !! TODO: 
                                    !! implement max str len for string stuff
                                    !! fix string implementation below (need 2D spec + max str len!)
                                    !! check here to make sure things work, then do 2D stuff!
                                    
                                    !! NOTE THAT THIS IS ON LOCAL MACHINE, AFTER DONE TRANSFER TO DALI AND
                                    !! CONTINUE DEV/TESTING THERE!!!
                                    
                                    mpi_requests_total = mpi_requests_total + 1
                                    
                                    if (tmp_var_type == NF90_BYTE) then
                                        allocate(temp_storage_arr(mpi_requests_total)%byte_buffer   (cur_dim_sizes(1)))
                                        ! EMPTY FILL GOES HERE
                                        temp_storage_arr(mpi_requests_total)%byte_buffer = NF90_FILL_BYTE
                                        call check(nf90_get_var(ncid_input, var_index, &
                                            temp_storage_arr(mpi_requests_total)%byte_buffer))
                                        
                                        ! Args: the variable, number of elements to send,
                                        ! data type (in MPI land), destination process #,
                                        ! numeric tag for extra info, and communicator.
                                        call MPI_ISend(temp_storage_arr(mpi_requests_total)%byte_buffer, &
                                            cur_dim_sizes(1), MPI_BYTE, &
                                            0, cur_out_var_ind, MPI_COMM_WORLD, &
                                            mpi_requests(mpi_requests_total), ierr)
                                        
                                        !data_blobs(cur_out_var_ind)%byte_buffer &
                                        !    (data_blobs(cur_out_var_ind)%cur_pos : &
                                        !        data_blobs(cur_out_var_ind)%cur_pos + cur_dim_sizes(1) - 1) &
                                        !    = byte_buffer(:)
                                        !deallocate(byte_buffer)
                                    else if (tmp_var_type == NF90_SHORT) then
                                        allocate(temp_storage_arr(mpi_requests_total)%short_buffer  (cur_dim_sizes(1)))
                                        temp_storage_arr(mpi_requests_total)%short_buffer = NF90_FILL_SHORT
                                        call check(nf90_get_var(ncid_input, var_index, &
                                            temp_storage_arr(mpi_requests_total)%short_buffer))
                                        
                                        call MPI_ISend(temp_storage_arr(mpi_requests_total)%short_buffer, &
                                            cur_dim_sizes(1), MPI_SHORT, &
                                            0, cur_out_var_ind, MPI_COMM_WORLD, &
                                            mpi_requests(mpi_requests_total), ierr)
                                        
                                        !data_blobs(cur_out_var_ind)%short_buffer &
                                        !    (data_blobs(cur_out_var_ind)%cur_pos : &
                                        !        data_blobs(cur_out_var_ind)%cur_pos + cur_dim_sizes(1) - 1) &
                                        !    = short_buffer(:)
                                        !deallocate(short_buffer)
                                    else if (tmp_var_type == NF90_INT) then
                                        allocate(temp_storage_arr(mpi_requests_total)%long_buffer   (cur_dim_sizes(1)))
                                        temp_storage_arr(mpi_requests_total)%long_buffer = NF90_FILL_INT
                                        call check(nf90_get_var(ncid_input, var_index, &
                                            temp_storage_arr(mpi_requests_total)%long_buffer))
                                        
                                        !write (*, "(A, I0, A, I0)") "[PROC ", cur_proc, &
                                        !    "] long_buffer send for var " // trim(var_names(cur_out_var_ind)) // ": ", &
                                        !    cur_dim_sizes(1)
                                        
                                        call MPI_ISend(temp_storage_arr(mpi_requests_total)%long_buffer, &
                                            cur_dim_sizes(1), MPI_INT, &
                                            0, cur_out_var_ind, MPI_COMM_WORLD, &
                                            mpi_requests(mpi_requests_total), ierr)
                                        
                                        !data_blobs(cur_out_var_ind)%long_buffer &
                                        !    (data_blobs(cur_out_var_ind)%cur_pos : &
                                        !        data_blobs(cur_out_var_ind)%cur_pos + cur_dim_sizes(1) - 1) &
                                        !    = long_buffer(:)
                                        !deallocate(long_buffer)
                                    else if (tmp_var_type == NF90_FLOAT) then
                                        allocate(temp_storage_arr(mpi_requests_total)%rsingle_buffer(cur_dim_sizes(1)))
                                        temp_storage_arr(mpi_requests_total)%rsingle_buffer = NF90_FILL_FLOAT
                                        call check(nf90_get_var(ncid_input, var_index, &
                                            temp_storage_arr(mpi_requests_total)%rsingle_buffer, &
                                            start = (/ 1 /), &
                                            count = (/ cur_dim_sizes(1) /) ))
                                        
                                        call MPI_ISend(temp_storage_arr(mpi_requests_total)%rsingle_buffer, &
                                            cur_dim_sizes(1), MPI_FLOAT, &
                                            0, cur_out_var_ind, MPI_COMM_WORLD, &
                                            mpi_requests(mpi_requests_total), ierr)
                                        
                                        !data_blobs(cur_out_var_ind)%rsingle_buffer &
                                        !    (data_blobs(cur_out_var_ind)%cur_pos : &
                                        !        data_blobs(cur_out_var_ind)%cur_pos + cur_dim_sizes(1) - 1) &
                                        !    = rsingle_buffer(:)
                                        !deallocate(rsingle_buffer)
                                    else if (tmp_var_type == NF90_DOUBLE) then
                                        allocate(temp_storage_arr(mpi_requests_total)%rdouble_buffer(cur_dim_sizes(1)))
                                        temp_storage_arr(mpi_requests_total)%rdouble_buffer = NF90_FILL_DOUBLE
                                        !print *, cur_dim_sizes(1)
                                        call check(nf90_get_var(ncid_input, var_index, &
                                            temp_storage_arr(mpi_requests_total)%rdouble_buffer, &
                                            start = (/ 1 /), &
                                            count = (/ cur_dim_sizes(1) /) ))
                                        
                                        call MPI_ISend(temp_storage_arr(mpi_requests_total)%rdouble_buffer, &
                                            cur_dim_sizes(1), MPI_DOUBLE, &
                                            0, cur_out_var_ind, MPI_COMM_WORLD, &
                                            mpi_requests(mpi_requests_total), ierr)
                                        
                                        !data_blobs(cur_out_var_ind)%rdouble_buffer &
                                        !    (data_blobs(cur_out_var_ind)%cur_pos : &
                                        !        data_blobs(cur_out_var_ind)%cur_pos + cur_dim_sizes(1) - 1) &
                                        !    = rdouble_buffer(:)
                                        !deallocate(rdouble_buffer)
                                    else if (tmp_var_type == NF90_CHAR) then
                                        allocate(string_buffer   (cur_dim_sizes(1), cur_dim_sizes(2)))
                                        
                                        ! NOTE: the 2nd dim is nobs, so this is the actual file size.
                                        ! Other fields are the final sizes (maximum).
                                        allocate(string_expanded_buffer (cur_out_dim_sizes(1), cur_dim_sizes(2)))
                                        
                                        ! Same again, this time just multiplying...
                                        !allocate(temp_storage_arr(mpi_requests_total)%string_1d_buffer(cur_out_dim_sizes(1)* cur_dim_sizes(2)))
                                        
                                        string_buffer = NF90_FILL_CHAR
                                        string_expanded_buffer = NF90_FILL_CHAR
                                        call check(nf90_get_var(ncid_input, var_index, string_buffer, &
                                            start = (/ 1, 1 /), &
                                            count = (/ cur_dim_sizes(1), cur_dim_sizes(2) /) ))
                                        
                                        string_expanded_buffer(1:cur_dim_sizes(1), 1:cur_dim_sizes(2)) = &
                                            string_buffer
                                        
                                        !temp_storage_arr(mpi_requests_total)%string_1d_buffer = reshape(string_buffer, &
                                        !    (/ cur_out_dim_sizes(1)* cur_dim_sizes(2) /))
                                        
                                        call MPI_ISend(temp_storage_arr(mpi_requests_total)%string_expanded_buffer, &
                                            cur_out_dim_sizes(1)* cur_dim_sizes(2), MPI_BYTE, &
                                            0, cur_out_var_ind, MPI_COMM_WORLD, &
                                            mpi_requests(mpi_requests_total), ierr)
                                        
                                        !data_blobs(cur_out_var_ind)%string_buffer &
                                        !    (data_blobs(cur_out_var_ind)%cur_pos : &
                                        !        data_blobs(cur_out_var_ind)%cur_pos + cur_dim_sizes(1) - 1, :) &
                                        !    = string_buffer(:,:)
                                        
                                        !deallocate(string_1d_buffer)
                                        deallocate(string_buffer)
                                        !deallocate(string_expanded_buffer)
                                        !write (*, "(A)") "VARIABLE: " // trim(var_names(cur_out_var_ind))
                                        !print *, "ALLOC SIZES: ", data_blobs(cur_out_var_ind)%alloc_size
                                        !write (*, "(A, I0, A, I0)") "start = ", data_blobs(cur_out_var_ind)%cur_pos, &
                                        !    " | end = ", data_blobs(cur_out_var_ind)%cur_pos + cur_dim_sizes(2) - 1
                                        !write (*, "(A, I0, A, I0)") "var dim 1 = ", cur_dim_sizes(1), &
                                        !    " | var dim 2 = ", cur_dim_sizes(2)
                                        !data_blobs(cur_out_var_ind)%string_buffer &
                                        !    (1 : cur_dim_sizes(1), &
                                        !        data_blobs(cur_out_var_ind)%cur_pos : &
                                        !        data_blobs(cur_out_var_ind)%cur_pos + cur_dim_sizes(2) - 1) &
                                        !            = NF90_FILL_CHAR
                                        !call check(nf90_get_var(ncid_input, var_index, &
                                        !    data_blobs(cur_out_var_ind)%string_buffer &
                                        !        (1 : data_blobs(cur_out_var_ind)%alloc_size(1), &
                                        !            data_blobs(cur_out_var_ind)%cur_pos : &
                                        !            data_blobs(cur_out_var_ind)%cur_pos + cur_dim_sizes(2) - 1), &
                                        !    start = (/ 1, 1 /), &
                                        !    count = (/ cur_dim_sizes(1), cur_dim_sizes(2) /) ))
                                        
                                        ! ALTERNATIVE USING INDEXING
                                        
                                        !do i = data_blobs(cur_out_var_ind)%cur_pos, &
                                        !    data_blobs(cur_out_var_ind)%cur_pos + cur_dim_sizes(2) - 1
                                        !    call check(nf90_get_var(ncid_input, var_index, &
                                        !        data_blobs(cur_out_var_ind)%string_buffer &
                                        !            (1 : cur_dim_sizes(1), &
                                        !                i), &
                                        !        start = (/ 1, 1 /), &
                                        !        count = (/ cur_dim_sizes(1), 1 /) ))
                                        !end do
                                        
                                        !allocate(tmp_string_buffer (cur_dim_sizes(1), cur_dim_sizes(2)))
                                        !tmp_string_buffer = NF90_FILL_CHAR
                                        !
                                        !call check(nf90_get_var(ncid_input, var_index, tmp_string_buffer, &
                                        !    start = (/ 1, 1 /), &
                                        !    count = (/ cur_dim_sizes(1), cur_dim_sizes(2) /) ))
                                        !
                                        !data_blobs(cur_out_var_ind)%string_buffer &
                                        !    (1 : cur_dim_sizes(1), &
                                        !        data_blobs(cur_out_var_ind)%cur_pos : &
                                        !        data_blobs(cur_out_var_ind)%cur_pos + cur_dim_sizes(2) - 1) = &
                                        !    tmp_string_buffer
                                        !
                                        !deallocate(tmp_string_buffer)
                                        
                                        !do i = data_blobs(cur_out_var_ind)%cur_pos, &
                                        !    data_blobs(cur_out_var_ind)%cur_pos + cur_dim_sizes(2) - 1
                                        !    
                                        !    write (*, "(I0, A)", advance = "no") i, "    ["
                                        !    
                                        !    do j = 1, data_blobs(cur_out_var_ind)%alloc_size(1)
                                        !        write (*, "(A)", advance = "no") &
                                        !            data_blobs(cur_out_var_ind)%string_buffer(j, i)
                                        !    end do
                                        !    write (*, "(A)") "]"
                                        !end do
                                    else
                                        write (err_string, "(A, I0, A)") &
                                            "Invalid type detected during write." // &
                                            CHAR(10) // "             " // &
                                            "(Variable '" // trim(tmp_var_name) // "' has an type of ", &
                                            tmp_var_type, "," // &
                                            CHAR(10) // "             " // &
                                            "which is invalid!)"
                                        call error(trim(err_string))
                                    end if
                                else if ((cur_out_var_ndims == 2) .OR. &
                                    ((cur_out_var_ndims == 3) .AND. (tmp_var_type == NF90_CHAR))) then
                                    
                                    mpi_requests_total = mpi_requests_total + 1
                                    
                                    if (tmp_var_type == NF90_BYTE) then
                                        allocate(temp_storage_arr(mpi_requests_total)%byte_2d_buffer   (cur_dim_sizes(1), cur_dim_sizes(2)))
                                        !allocate(temp_storage_arr(mpi_requests_total)%byte_buffer      (cur_dim_sizes(1)* cur_dim_sizes(2)))
                                        temp_storage_arr(mpi_requests_total)%byte_2d_buffer = NF90_FILL_BYTE
                                        call check(nf90_get_var(ncid_input, var_index, &
                                            temp_storage_arr(mpi_requests_total)%byte_2d_buffer))
                                        !temp_storage_arr(mpi_requests_total)%byte_buffer = &
                                        !    reshape(byte_2d_buffer, (/ cur_dim_sizes(1)* cur_dim_sizes(2) /))
                                        
                                        call MPI_ISend(temp_storage_arr(mpi_requests_total)%byte_2d_buffer, &
                                            cur_dim_sizes(1)* cur_dim_sizes(2), MPI_BYTE, &
                                            0, cur_out_var_ind, MPI_COMM_WORLD, &
                                            mpi_requests(mpi_requests_total), ierr)
                                        
                                        !data_blobs(cur_out_var_ind)%byte_2d_buffer &
                                        !    (1 : cur_dim_sizes(1), &
                                        !        data_blobs(cur_out_var_ind)%cur_pos : &
                                        !        data_blobs(cur_out_var_ind)%cur_pos + cur_dim_sizes(2) - 1) &
                                        !    = byte_2d_buffer(:,:)
                                        
                                        !deallocate(byte_buffer)
                                        !deallocate(byte_2d_buffer)
                                    else if (tmp_var_type == NF90_SHORT) then
                                        allocate(temp_storage_arr(mpi_requests_total)%short_2d_buffer  (cur_dim_sizes(1), cur_dim_sizes(2)))
                                        !allocate(temp_storage_arr(mpi_requests_total)%short_buffer     (cur_dim_sizes(1)* cur_dim_sizes(2)))
                                        temp_storage_arr(mpi_requests_total)%short_2d_buffer = NF90_FILL_SHORT
                                        call check(nf90_get_var(ncid_input, var_index, &
                                            temp_storage_arr(mpi_requests_total)%short_2d_buffer))
                                        !temp_storage_arr(mpi_requests_total)%short_buffer = &
                                        !    reshape(short_2d_buffer, (/ cur_dim_sizes(1)* cur_dim_sizes(2) /))
                                        
                                        call MPI_ISend(temp_storage_arr(mpi_requests_total)%short_2d_buffer, &
                                            cur_dim_sizes(1)* cur_dim_sizes(2), MPI_SHORT, &
                                            0, cur_out_var_ind, MPI_COMM_WORLD, &
                                            mpi_requests(mpi_requests_total), ierr)
                                        
                                        !data_blobs(cur_out_var_ind)%short_2d_buffer &
                                        !    (1 : cur_dim_sizes(1), &
                                        !        data_blobs(cur_out_var_ind)%cur_pos : &
                                        !        data_blobs(cur_out_var_ind)%cur_pos + cur_dim_sizes(2) - 1) &
                                        !    = short_2d_buffer(:,:)
                                        
                                        !deallocate(short_buffer)
                                        !deallocate(short_2d_buffer)
                                    else if (tmp_var_type == NF90_INT) then
                                        allocate(temp_storage_arr(mpi_requests_total)%long_2d_buffer   (cur_dim_sizes(1), cur_dim_sizes(2)))
                                        !allocate(temp_storage_arr(mpi_requests_total)%long_buffer      (cur_dim_sizes(1)* cur_dim_sizes(2)))
                                        temp_storage_arr(mpi_requests_total)%long_2d_buffer = NF90_FILL_INT
                                        call check(nf90_get_var(ncid_input, var_index, &
                                            temp_storage_arr(mpi_requests_total)%long_2d_buffer))
                                        !temp_storage_arr(mpi_requests_total)%long_buffer = &
                                        !    reshape(long_2d_buffer, (/ cur_dim_sizes(1)* cur_dim_sizes(2) /))
                                        
                                        call MPI_ISend(temp_storage_arr(mpi_requests_total)%long_2d_buffer, &
                                            cur_dim_sizes(1)* cur_dim_sizes(2), MPI_INT, &
                                            0, cur_out_var_ind, MPI_COMM_WORLD, &
                                            mpi_requests(mpi_requests_total), ierr)
                                        
                                        !data_blobs(cur_out_var_ind)%long_2d_buffer &
                                        !    (1 : cur_dim_sizes(1), &
                                        !        data_blobs(cur_out_var_ind)%cur_pos : &
                                        !        data_blobs(cur_out_var_ind)%cur_pos + cur_dim_sizes(2) - 1) &
                                        !    = long_2d_buffer(:,:)
                                        
                                        !deallocate(long_buffer)
                                        !deallocate(long_2d_buffer)
                                    else if (tmp_var_type == NF90_FLOAT) then
                                        allocate(temp_storage_arr(mpi_requests_total)%rsingle_2d_buffer(cur_dim_sizes(1), cur_dim_sizes(2)))
                                        !allocate(temp_storage_arr(mpi_requests_total)%rsingle_buffer   (cur_dim_sizes(1)* cur_dim_sizes(2)))
                                        temp_storage_arr(mpi_requests_total)%rsingle_2d_buffer = NF90_FILL_FLOAT
                                        call check(nf90_get_var(ncid_input, var_index, &
                                            temp_storage_arr(mpi_requests_total)%rsingle_2d_buffer, &
                                            start = (/ 1, 1 /), &
                                            count = (/ cur_dim_sizes(1), cur_dim_sizes(2) /) ))
                                        !temp_storage_arr(mpi_requests_total)%rsingle_buffer = &
                                        !    reshape(rsingle_2d_buffer, (/ cur_dim_sizes(1)* cur_dim_sizes(2) /))
                                        
                                        call MPI_ISend(temp_storage_arr(mpi_requests_total)%rsingle_2d_buffer, &
                                            cur_dim_sizes(1)* cur_dim_sizes(2), MPI_FLOAT, &
                                            0, cur_out_var_ind, MPI_COMM_WORLD, &
                                            mpi_requests(mpi_requests_total), ierr)
                                        
                                        !data_blobs(cur_out_var_ind)%rsingle_2d_buffer &
                                        !    (1 : cur_dim_sizes(1), &
                                        !        data_blobs(cur_out_var_ind)%cur_pos : &
                                        !        data_blobs(cur_out_var_ind)%cur_pos + cur_dim_sizes(2) - 1) &
                                        !    = rsingle_2d_buffer(:,:)
                                        
                                        !deallocate(rsingle_buffer)
                                        !deallocate(rsingle_2d_buffer)
                                    else if (tmp_var_type == NF90_DOUBLE) then
                                        allocate(temp_storage_arr(mpi_requests_total)%rdouble_2d_buffer(cur_dim_sizes(1), cur_dim_sizes(2)))
                                        !allocate(temp_storage_arr(mpi_requests_total)%rdouble_buffer   (cur_dim_sizes(1)* cur_dim_sizes(2)))
                                        temp_storage_arr(mpi_requests_total)%rdouble_2d_buffer = NF90_FILL_DOUBLE
                                        call check(nf90_get_var(ncid_input, var_index, &
                                            temp_storage_arr(mpi_requests_total)%rdouble_2d_buffer, &
                                            start = (/ 1, 1 /), &
                                            count = (/ cur_dim_sizes(1), cur_dim_sizes(2) /) ))
                                        !temp_storage_arr(mpi_requests_total)%rdouble_buffer = &
                                        !    reshape(rdouble_2d_buffer, (/ cur_dim_sizes(1)* cur_dim_sizes(2) /))
                                        
                                        call MPI_ISend(temp_storage_arr(mpi_requests_total)%rdouble_2d_buffer, &
                                            cur_dim_sizes(1)* cur_dim_sizes(2), MPI_DOUBLE, &
                                            0, cur_out_var_ind, MPI_COMM_WORLD, &
                                            mpi_requests(mpi_requests_total), ierr)
                                        
                                        !data_blobs(cur_out_var_ind)%rdouble_2d_buffer &
                                        !    (1 : cur_dim_sizes(1), &
                                        !        data_blobs(cur_out_var_ind)%cur_pos : &
                                        !        data_blobs(cur_out_var_ind)%cur_pos + cur_dim_sizes(2) - 1) &
                                        !    = rdouble_2d_buffer(:,:)
                                        
                                        !deallocate(rdouble_buffer)
                                        !deallocate(rdouble_2d_buffer)
                                    else if (tmp_var_type == NF90_CHAR) then
                                        allocate(string_2d_buffer (cur_dim_sizes(1), cur_dim_sizes(2), cur_dim_sizes(3)))
                                        
                                        ! NOTE: the 3rd dim is nobs, so this is the actual file size.
                                        ! Other fields are the final sizes (maximum).
                                        allocate(temp_storage_arr(mpi_requests_total)%string_2d_expanded_buffer &
                                            (cur_out_dim_sizes(1), cur_out_dim_sizes(2), cur_dim_sizes(3)))
                                        
                                        ! Same again, this time just multiplying...
                                        !allocate(temp_storage_arr(mpi_requests_total)%string_1d_buffer (cur_out_dim_sizes(1)* cur_out_dim_sizes(2)* cur_dim_sizes(3)))
                                        string_2d_buffer = NF90_FILL_CHAR
                                        string_2d_expanded_buffer = NF90_FILL_CHAR
                                        call check(nf90_get_var(ncid_input, var_index, string_2d_buffer, &
                                            start = (/ 1, 1, 1 /), &
                                            count = (/ cur_dim_sizes(1), cur_dim_sizes(2), cur_dim_sizes(3) /) ))
                                        
                                        temp_storage_arr(mpi_requests_total)%string_2d_expanded_buffer &
                                            (1:cur_dim_sizes(1), 1:cur_dim_sizes(2), 1:cur_dim_sizes(3)) = &
                                                string_2d_buffer
                                        
                                        !temp_storage_arr(mpi_requests_total)%string_1d_buffer = &
                                        !    reshape(string_2d_expanded_buffer, &
                                        !        (/ cur_out_dim_sizes(1)* cur_out_dim_sizes(2)* cur_dim_sizes(3) /))
                                        !data_blobs(cur_out_var_ind)%string_2d_buffer(data_blobs(cur_out_var_ind)%cur_pos:,:,:) &
                                        !    = string_2d_buffer(:,:,:)
                                        
                                        call MPI_ISend(temp_storage_arr(mpi_requests_total)%string_expanded_buffer, &
                                            cur_out_dim_sizes(1)* cur_out_dim_sizes(2)* cur_dim_sizes(3), MPI_BYTE, &
                                            0, cur_out_var_ind, MPI_COMM_WORLD, &
                                            mpi_requests(mpi_requests_total), ierr)
                                        
                                        !data_blobs(cur_out_var_ind)%string_2d_buffer &
                                        !    (1 : cur_dim_sizes(1), 1 : cur_dim_sizes(2), &
                                        !        data_blobs(cur_out_var_ind)%cur_pos : &
                                        !        data_blobs(cur_out_var_ind)%cur_pos + cur_dim_sizes(3) - 1) & &
                                        !    = string_2d_buffer(:,:,:)
                                        
                                        !call check(nf90_put_var(ncid_output, cur_out_var_id, string_2d_buffer, &
                                        !    start = (/ 1, 1, 1 + dim_counters(nc_diag_cat_lookup_dim(tmp_var_dim_names(3))) /), &
                                        !    count = (/ cur_dim_sizes(1), cur_dim_sizes(2), cur_dim_sizes(3) /) ))
                                        !deallocate(string_1d_buffer)
                                        deallocate(string_2d_buffer)
                                        !deallocate(string_2d_expanded_buffer)
                                    else
                                        write (err_string, "(A, I0, A)") &
                                            "Invalid type detected during write." // &
                                            CHAR(10) // "             " // &
                                            "(Variable '" // trim(tmp_var_name) // "' has an type of ", &
                                            tmp_var_type, "," // &
                                            CHAR(10) // "             " // &
                                            "which is invalid!)"
                                        call error(trim(err_string))
                                    end if
                                end if
                                
                                !print *, "cur_out_var_ndims", cur_out_var_ndims
                                !print *, "cur_dim_sizes(cur_out_var_ndims)", cur_dim_sizes(cur_out_var_ndims)
                                !print *, "any(cur_out_dim_sizes == -1)", any(cur_out_dim_sizes == -1)
                                
                                !if (any(cur_out_dim_sizes == -1)) &
                                !    data_blobs(cur_out_var_ind)%cur_pos = &
                                !        data_blobs(cur_out_var_ind)%cur_pos + cur_dim_sizes(cur_out_var_ndims)
                                
                                var_counters(cur_out_var_ind) = &
                                        var_counters(cur_out_var_ind) + 1
                            end if
                            
                            ! Deallocate
                            deallocate(tmp_var_dimids)
                            deallocate(tmp_var_dim_names)
                            deallocate(cur_dim_ids)
                            deallocate(cur_dim_sizes)
                            deallocate(cur_out_dim_ids)
                            deallocate(cur_out_dim_sizes)
                        end do
                        
                        ! Update any unlimited counters
                        if (any(dim_sizes == -1)) then
                            do i = 1, dim_arr_total
                                ! Check for -1 - unlimited indicator
                                if ((dim_sizes(i) == -1) .AND. (any(tmp_in_dim_names == dim_names(i)))) then
                                    ! We got one! But... we need to find this dimension in the file.
                                    ! First, lookup dimension name to get dimension ID.
                                    call check(nf90_inq_dimid(ncid_input, dim_names(i), cur_dim_id))
                                    
                                    ! Then, grab the current unlimited dimension length!
                                    call check(nf90_inquire_dimension(ncid_input, cur_dim_id, len = cur_dim_len))
                                    
                                    ! Add the length to the counter!
                                    dim_counters(i) = dim_counters(i) + cur_dim_len
                                end if
                            end do
                        end if
                        
                        call check(nf90_close(ncid_input))
                        
                        !deallocate(unlim_dims)
                        !deallocate(tmp_input_dimids)
                        deallocate(tmp_input_varids)
                        deallocate(tmp_in_dim_names)
                    end if
                    
                    ! Send EOF notification
                    mpi_requests_total = mpi_requests_total + 1
                    call MPI_ISend(0, 1, MPI_INT, 0, mpi_request_EOF, MPI_COMM_WORLD, &
                        mpi_requests(mpi_requests_total), ierr)
                end do
                
                ! Send process competion notification
                mpi_requests_total = mpi_requests_total + 1
                call MPI_ISend(0, 1, MPI_INT, 0, mpi_request_EOP, MPI_COMM_WORLD, &
                    mpi_requests(mpi_requests_total), ierr)
                
                ! Flush all MPI communications!
                ! (Deallocate everything while we're at it!)
                call info(" -> Flushing all data...")
                do i = 1, mpi_requests_total
                    call MPI_Wait(mpi_requests(i), mpi_status, ierr)
                end do
                
                ! This will deallocate everything, including internal stuff
                deallocate(temp_storage_arr)
            else
                ! Do collection!
                ! We know how much we need to receive - but we don't know
                ! when we will get our data. So we'll keep a tally of
                ! what data we got, and once we reach our limit, we'll be
                ! done!
                
                allocate(read_var_count(var_arr_total))
                read_var_count = 0
                
                allocate(procs_done_arr(num_procs - 1))
                procs_done_arr = -1
                
                procs_done = 0
                
                !print *, "VAR COUNTERS PROC 2 info: ", var_counters
                call info("Receiving data from other processes...")
                !print *, "PROC 2 info | var_counters(1) = ", var_counters(1)
                base_proc = 1
                do while (file_count /= input_count)
                    !print *, "PROC VAR COUNTERS:"
                    !print *, var_counters(1:var_arr_total)
                    !print *, "FILE COUNT:"
                    !print *, file_count
                    !print *, "INPUT COUNT:"
                    !print *, input_count
                    !print *, "PROC 2 info | again, var_counters(1) = ", var_counters(1)
                    do i_proc = 1, num_procs - 1
                        ! Make sure this process isn't already done
                        if (any(procs_done_arr == i_proc)) &
                            cycle
                        
                        !print *, "PROC 2 info | in i_proc loop 1 | var_counters(1) = ", var_counters(1)
                        !print *, "Probing proc: ", i_proc
                        
                        call MPI_Probe(i_proc, MPI_ANY_TAG, MPI_COMM_WORLD, mpi_status, ierr)
                        !call MPI_Iprobe(i_proc, MPI_ANY_TAG, MPI_COMM_WORLD, mpi_read_flag, mpi_status, ierr)
                        
                        ! Skip the rest if we can't do anything!
                        !if (.NOT. mpi_read_flag) then
                        !    base_proc = i_proc
                        !    exit
                        !end if
                        
                        !print *, "PROC 2 info | in i_proc loop 2 | var_counters(1) = ", var_counters(1)
                        if (ierr /= 0) &
                            call error("MPI ERROR OCCURRED!")
                        
                        ! Within mpi_status, we get the following:
                        !   MPI_SOURCE - the source process #
                        !   MPI_TAG    - the tag # for the index
                        !   MPI_ERROR  - error code of the probe operation
                        ! 
                        ! The above are also indexes to fetch said value, e.g.
                        ! mpi_status(MPI_SOURCE) will get the source process #,
                        ! mpi_status(MPI_TAG) will get the tag #, etc.
                        ! 
                        ! We also have a hidden value that needs to be "decoded"
                        ! via MPI_GET_COUNT. This gives us the number of
                        ! elements we'll be fetching.
                        ! 
                        ! Now we gotta decode all of this into something we can use!
                        ! Our MPI_TAG is a big helper for us. MPI_TAG contains the
                        ! relative variable index that we use in our database.
                        ! As a result, we can instantly figure out the dimensions
                        ! and type for the variable we are about to store.
                        ! 
                        ! With this information, we can take the count and divide it
                        ! by the fixed dimensions we just got to get our unlimited
                        ! dimensions (if applicable).
                        ! 
                        ! Now, with the fixed and unlimited dimensions, the types,
                        ! and the variable index from the tag, we can now reconstruct
                        ! the data.
                        ! 
                        ! First and foremost, we need to allocate a temporary variable
                        ! to store all of our data. Once we've allocated, we'll fetch
                        ! the data given all the parameters we got, plus the allocated
                        ! temporary variable.
                        ! 
                        ! Once everything's done, we'll go ahead and add it to the final
                        ! array, with the correct position pre-calculated!
                        ! 
                        ! Note - strings WILL be expanded to the pre-computed size.
                        
                        ! Finished file tag
                        if (mpi_status(MPI_TAG) == var_arr_total + 1000) then
                            !write (*, "(A, I0)") "File done notification from proc ", i_proc
                            file_count = file_count + 1
                            !write (*, "(A, I0)") "File done changed file_count to:", file_count
                            call MPI_Recv(i, 1, MPI_INT, &
                                          i_proc, var_arr_total + 1000, MPI_COMM_WORLD, mpi_status, ierr)
                            cycle
                        end if
                        
                        ! Finished process tag
                        if (mpi_status(MPI_TAG) == var_arr_total + 2000) then
                            !write (*, "(A, I0)") "Process done notification from proc ", i_proc
                            call MPI_Recv(i, 1, MPI_INT, &
                                          i_proc, var_arr_total + 2000, MPI_COMM_WORLD, mpi_status, ierr)
                            procs_done = procs_done + 1
                            procs_done_arr(procs_done) = i_proc
                            cycle
                        end if
                        
                        !write (*, "(A, I0)") "Processing var from proc ", i_proc
                        cur_out_var_ind = mpi_status(MPI_TAG)
                        !print *, "PROC 2 info | in i_proc loop 3 | var_counters(1) = ", var_counters(1)
                        cur_out_var_ndims = var_dim_names(cur_out_var_ind)%num_names
                        !print *, "PROC 2 info | in i_proc loop 4 | var_counters(1) = ", var_counters(1)
                        allocate(cur_out_dim_sizes(cur_out_var_ndims))
                        !write (*, "(A, I0, A)") "PROC ", i_proc, " sent variable " // var_names(cur_out_var_ind)
                        !write (*, "(A, I0, A, I0)") "PROC ", i_proc, " wrote index cur_out_var_ind = ", cur_out_var_ind
                        !write (*, "(A, I0, A, I0)") "PROC ", i_proc, " var_counters(cur_out_var_ind) = ", var_counters(cur_out_var_ind)
                        !print *, "PROC 2 info | in i_proc loop 5 | var_counters(1) = ", var_counters(1)
                        cur_out_var_counter = var_counters(cur_out_var_ind)
                        !print *, "PROC 2 info | in i_proc loop 6 | var_counters(1) = ", var_counters(1)
                        cur_out_var_type = var_types(cur_out_var_ind)
                        
                        do i = 1, cur_out_var_ndims
                            cur_out_dim_ind = nc_diag_cat_lookup_dim(var_dim_names(cur_out_var_ind)%dim_names(i))
                            cur_out_dim_sizes(i) = dim_sizes(cur_out_dim_ind)
                        end do
                        
                        !print *, "Variable " // trim(var_names(cur_out_var_ind)) // ": type = ", cur_out_var_type
                        
                        if (cur_out_var_type == NF90_BYTE)   call MPI_GET_COUNT(mpi_status, MPI_BYTE, num_count, ierr)
                        if (cur_out_var_type == NF90_SHORT)  call MPI_GET_COUNT(mpi_status, MPI_SHORT, num_count, ierr)
                        if (cur_out_var_type == NF90_INT)    call MPI_GET_COUNT(mpi_status, MPI_INT, num_count, ierr)
                        if (cur_out_var_type == NF90_FLOAT)  call MPI_GET_COUNT(mpi_status, MPI_FLOAT, num_count, ierr)
                        if (cur_out_var_type == NF90_DOUBLE) call MPI_GET_COUNT(mpi_status, MPI_DOUBLE, num_count, ierr)
                        if (cur_out_var_type == NF90_CHAR)   call MPI_GET_COUNT(mpi_status, MPI_BYTE, num_count, ierr)
                        
                        if (ierr /= 0) &
                            call error("MPI ERROR OCCURRED!")
                        
                        !if (cur_out_var_type == NF90_BYTE)   print *, "NF90_BYTE"
                        !if (cur_out_var_type == NF90_SHORT)  print *, "NF90_SHORT"
                        !if (cur_out_var_type == NF90_INT)    print *, "NF90_INT"
                        !if (cur_out_var_type == NF90_FLOAT)  print *, "NF90_FLOAT"
                        !if (cur_out_var_type == NF90_DOUBLE) print *, "NF90_DOUBLE"
                        !if (cur_out_var_type == NF90_CHAR)   print *, "NF90_CHAR"
                        
                        !print *, "POGO"
                        !write (*, "(A, I0, A, I0, A, I0, A, I0)") "[PROC ", cur_proc, &
                        !                    " | RECV] PROC ", i_proc, &
                        !                    " sends var " // trim(var_names(cur_out_var_ind)) // ": incoming size ", &
                        !                    num_count, ", var_counter = ", cur_out_var_counter
                        
                        ! Check for one-time only vars...
                        if (((.NOT. any(cur_out_dim_sizes == -1)) .AND. (cur_out_var_counter == 0)) &
                            .OR. (any(cur_out_dim_sizes == -1))) then
                            
                            if ((cur_out_var_ndims == 1) .OR. &
                                ((cur_out_var_ndims == 2) .AND. (tmp_var_type == NF90_CHAR))) then
                                if (cur_out_var_type == NF90_BYTE) then
                                    allocate(byte_buffer   (num_count))
                                    byte_buffer = NF90_FILL_BYTE
                                    
                                    ! Args: the target variable, number of elements to recv,
                                    ! data type (in MPI land), source process #,
                                    ! numeric tag for extra info, and communicator.
                                    call MPI_Recv(byte_buffer, num_count, MPI_BYTE, &
                                        i_proc, cur_out_var_ind, MPI_COMM_WORLD, mpi_status, ierr)
                                    
                                    data_blobs(cur_out_var_ind)%byte_buffer &
                                        (data_blobs(cur_out_var_ind)%cur_pos : &
                                            data_blobs(cur_out_var_ind)%cur_pos + num_count - 1) &
                                        = byte_buffer(:)
                                    
                                    deallocate(byte_buffer)
                                else if (cur_out_var_type == NF90_SHORT) then
                                    allocate(short_buffer   (num_count))
                                    short_buffer = NF90_FILL_SHORT
                                    
                                    ! Args: the target variable, number of elements to recv,
                                    ! data type (in MPI land), source process #,
                                    ! numeric tag for extra info, and communicator.
                                    call MPI_Recv(short_buffer, num_count, MPI_SHORT, &
                                        i_proc, cur_out_var_ind, MPI_COMM_WORLD, mpi_status, ierr)
                                    
                                    data_blobs(cur_out_var_ind)%short_buffer &
                                        (data_blobs(cur_out_var_ind)%cur_pos : &
                                            data_blobs(cur_out_var_ind)%cur_pos + num_count - 1) &
                                        = short_buffer(:)
                                    
                                    deallocate(short_buffer)
                                else if (cur_out_var_type == NF90_INT) then
                                    allocate(long_buffer   (num_count))
                                    long_buffer = NF90_FILL_INT
                                    
                                    !write (*, "(A, I0, A, I0)") "[PROC ", cur_proc, &
                                    !        " | RECV] long_buffer recv for var " // trim(var_names(cur_out_var_ind)) // ": ", &
                                    !        num_count
                                    
                                    ! Args: the target variable, number of elements to recv,
                                    ! data type (in MPI land), source process #,
                                    ! numeric tag for extra info, and communicator.
                                    call MPI_Recv(long_buffer, num_count, MPI_INT, &
                                        i_proc, cur_out_var_ind, MPI_COMM_WORLD, mpi_status, ierr)
                                    
                                    data_blobs(cur_out_var_ind)%long_buffer &
                                        (data_blobs(cur_out_var_ind)%cur_pos : &
                                            data_blobs(cur_out_var_ind)%cur_pos + num_count - 1) &
                                        = long_buffer(:)
                                    
                                    deallocate(long_buffer)
                                else if (cur_out_var_type == NF90_FLOAT) then
                                    allocate(rsingle_buffer   (num_count))
                                    rsingle_buffer = NF90_FILL_FLOAT
                                    
                                    ! Args: the target variable, number of elements to recv,
                                    ! data type (in MPI land), source process #,
                                    ! numeric tag for extra info, and communicator.
                                    call MPI_Recv(rsingle_buffer, num_count, MPI_FLOAT, &
                                        i_proc, cur_out_var_ind, MPI_COMM_WORLD, mpi_status, ierr)
                                    
                                    data_blobs(cur_out_var_ind)%rsingle_buffer &
                                        (data_blobs(cur_out_var_ind)%cur_pos : &
                                            data_blobs(cur_out_var_ind)%cur_pos + num_count - 1) &
                                        = rsingle_buffer(:)
                                    
                                    deallocate(rsingle_buffer)
                                else if (cur_out_var_type == NF90_DOUBLE) then
                                    allocate(rdouble_buffer(num_count))
                                    rdouble_buffer = NF90_FILL_DOUBLE
                                    
                                    ! Args: the target variable, number of elements to recv,
                                    ! data type (in MPI land), source process #,
                                    ! numeric tag for extra info, and communicator.
                                    call MPI_Recv(rdouble_buffer, num_count, MPI_DOUBLE, &
                                        i_proc, cur_out_var_ind, MPI_COMM_WORLD, mpi_status, ierr)
                                    
                                    data_blobs(cur_out_var_ind)%rdouble_buffer &
                                        (data_blobs(cur_out_var_ind)%cur_pos : &
                                            data_blobs(cur_out_var_ind)%cur_pos + num_count - 1) &
                                        = rdouble_buffer(:)
                                    
                                    deallocate(rdouble_buffer)
                                else if (cur_out_var_type == NF90_CHAR) then
                                    allocate(string_buffer   (cur_out_dim_sizes(1), num_count / cur_out_dim_sizes(1)))
                                    
                                    string_buffer = NF90_FILL_CHAR
                                    
                                    ! Args: the target variable, number of elements to recv,
                                    ! data type (in MPI land), source process #,
                                    ! numeric tag for extra info, and communicator.
                                    call MPI_Recv(string_buffer, num_count, MPI_BYTE, &
                                        i_proc, cur_out_var_ind, MPI_COMM_WORLD, mpi_status, ierr)
                                    
                                    data_blobs(cur_out_var_ind)%string_buffer &
                                        (data_blobs(cur_out_var_ind)%cur_pos : &
                                            data_blobs(cur_out_var_ind)%cur_pos + num_count - 1, :) &
                                        = string_buffer(:,:)
                                    
                                    deallocate(string_buffer)
                                    !write (*, "(A)") "VARIABLE: " // trim(var_names(cur_out_var_ind))
                                    !print *, "ALLOC SIZES: ", data_blobs(cur_out_var_ind)%alloc_size
                                    !write (*, "(A, I0, A, I0)") "start = ", data_blobs(cur_out_var_ind)%cur_pos, &
                                    !    " | end = ", data_blobs(cur_out_var_ind)%cur_pos + cur_dim_sizes(2) - 1
                                    !write (*, "(A, I0, A, I0)") "var dim 1 = ", cur_dim_sizes(1), &
                                    !    " | var dim 2 = ", cur_dim_sizes(2)
                                    !data_blobs(cur_out_var_ind)%string_buffer &
                                    !    (1 : cur_dim_sizes(1), &
                                    !        data_blobs(cur_out_var_ind)%cur_pos : &
                                    !        data_blobs(cur_out_var_ind)%cur_pos + cur_dim_sizes(2) - 1) &
                                    !            = NF90_FILL_CHAR
                                    !call check(nf90_get_var(ncid_input, var_index, &
                                    !    data_blobs(cur_out_var_ind)%string_buffer &
                                    !        (1 : data_blobs(cur_out_var_ind)%alloc_size(1), &
                                    !            data_blobs(cur_out_var_ind)%cur_pos : &
                                    !            data_blobs(cur_out_var_ind)%cur_pos + cur_dim_sizes(2) - 1), &
                                    !    start = (/ 1, 1 /), &
                                    !    count = (/ cur_dim_sizes(1), cur_dim_sizes(2) /) ))
                                    
                                    ! ALTERNATIVE USING INDEXING
                                    
                                    !do i = data_blobs(cur_out_var_ind)%cur_pos, &
                                    !    data_blobs(cur_out_var_ind)%cur_pos + cur_dim_sizes(2) - 1
                                    !    call check(nf90_get_var(ncid_input, var_index, &
                                    !        data_blobs(cur_out_var_ind)%string_buffer &
                                    !            (1 : cur_dim_sizes(1), &
                                    !                i), &
                                    !        start = (/ 1, 1 /), &
                                    !        count = (/ cur_dim_sizes(1), 1 /) ))
                                    !end do
                                    
                                    !allocate(tmp_string_buffer (cur_dim_sizes(1), cur_dim_sizes(2)))
                                    !tmp_string_buffer = NF90_FILL_CHAR
                                    !
                                    !call check(nf90_get_var(ncid_input, var_index, tmp_string_buffer, &
                                    !    start = (/ 1, 1 /), &
                                    !    count = (/ cur_dim_sizes(1), cur_dim_sizes(2) /) ))
                                    !
                                    !data_blobs(cur_out_var_ind)%string_buffer &
                                    !    (1 : cur_dim_sizes(1), &
                                    !        data_blobs(cur_out_var_ind)%cur_pos : &
                                    !        data_blobs(cur_out_var_ind)%cur_pos + cur_dim_sizes(2) - 1) = &
                                    !    tmp_string_buffer
                                    !
                                    !deallocate(tmp_string_buffer)
                                    
                                    !do i = data_blobs(cur_out_var_ind)%cur_pos, &
                                    !    data_blobs(cur_out_var_ind)%cur_pos + cur_dim_sizes(2) - 1
                                    !    
                                    !    write (*, "(I0, A)", advance = "no") i, "    ["
                                    !    
                                    !    do j = 1, data_blobs(cur_out_var_ind)%alloc_size(1)
                                    !        write (*, "(A)", advance = "no") &
                                    !            data_blobs(cur_out_var_ind)%string_buffer(j, i)
                                    !    end do
                                    !    write (*, "(A)") "]"
                                    !end do
                                else
                                    write (err_string, "(A, I0, A)") &
                                        "Invalid type detected during write." // &
                                        CHAR(10) // "             " // &
                                        "(Variable '" // trim(tmp_var_name) // "' has an type of ", &
                                        tmp_var_type, "," // &
                                        CHAR(10) // "             " // &
                                        "which is invalid!)"
                                    call error(trim(err_string))
                                end if
                                
                                if (any(cur_out_dim_sizes == -1)) then
                                    if (cur_out_var_type == NF90_CHAR) then
                                        data_blobs(cur_out_var_ind)%cur_pos = &
                                            data_blobs(cur_out_var_ind)%cur_pos + &
                                            (num_count / cur_out_dim_sizes(1))
                                    else
                                        data_blobs(cur_out_var_ind)%cur_pos = &
                                            data_blobs(cur_out_var_ind)%cur_pos + &
                                            num_count
                                    end if
                                end if
                            else if ((cur_out_var_ndims == 2) .OR. &
                                ((cur_out_var_ndims == 3) .AND. (cur_out_var_type == NF90_CHAR))) then
                                
                                if (cur_out_var_type == NF90_BYTE) then
                                    allocate(byte_2d_buffer   (cur_out_dim_sizes(1), num_count / cur_out_dim_sizes(1)))
                                    !allocate(byte_buffer      (num_count))
                                    byte_2d_buffer = NF90_FILL_BYTE
                                    
                                    ! Args: the target variable, number of elements to recv,
                                    ! data type (in MPI land), source process #,
                                    ! numeric tag for extra info, and communicator.
                                    call MPI_Recv(byte_2d_buffer, num_count, MPI_BYTE, &
                                        i_proc, cur_out_var_ind, MPI_COMM_WORLD, mpi_status, ierr)
                                    
                                    !byte_2d_buffer = reshape(byte_buffer, (/ cur_out_dim_sizes(1), num_count / cur_out_dim_sizes(1) /))
                                    
                                    data_blobs(cur_out_var_ind)%byte_2d_buffer &
                                        (1 : cur_out_dim_sizes(1), &
                                            data_blobs(cur_out_var_ind)%cur_pos : &
                                            data_blobs(cur_out_var_ind)%cur_pos + (num_count / cur_out_dim_sizes(1)) - 1) &
                                        = byte_2d_buffer(:,:)
                                    
                                    !deallocate(byte_buffer)
                                    deallocate(byte_2d_buffer)
                                else if (cur_out_var_type == NF90_SHORT) then
                                    allocate(short_2d_buffer  (cur_out_dim_sizes(1), num_count / cur_out_dim_sizes(1)))
                                    !allocate(short_buffer     (num_count))
                                    short_2d_buffer = NF90_FILL_SHORT
                                    
                                    ! Args: the target variable, number of elements to recv,
                                    ! data type (in MPI land), source process #,
                                    ! numeric tag for extra info, and communicator.
                                    call MPI_Recv(short_2d_buffer, num_count, MPI_SHORT, &
                                        i_proc, cur_out_var_ind, MPI_COMM_WORLD, mpi_status, ierr)
                                    
                                    !short_2d_buffer = reshape(short_buffer, (/ cur_out_dim_sizes(1), num_count / cur_out_dim_sizes(1) /))
                                    
                                    data_blobs(cur_out_var_ind)%short_2d_buffer &
                                        (1 : cur_out_dim_sizes(1), &
                                            data_blobs(cur_out_var_ind)%cur_pos : &
                                            data_blobs(cur_out_var_ind)%cur_pos + (num_count / cur_out_dim_sizes(1)) - 1) &
                                        = short_2d_buffer(:,:)
                                    
                                    !deallocate(short_buffer)
                                    deallocate(short_2d_buffer)
                                else if (cur_out_var_type == NF90_INT) then
                                    allocate(long_2d_buffer   (cur_out_dim_sizes(1), num_count / cur_out_dim_sizes(1)))
                                    !allocate(long_buffer      (num_count))
                                    long_2d_buffer = NF90_FILL_INT
                                    
                                    ! Args: the target variable, number of elements to recv,
                                    ! data type (in MPI land), source process #,
                                    ! numeric tag for extra info, and communicator.
                                    call MPI_Recv(long_2d_buffer, num_count, MPI_INT, &
                                        i_proc, cur_out_var_ind, MPI_COMM_WORLD, mpi_status, ierr)
                                    
                                    !long_2d_buffer = reshape(long_buffer, (/ cur_out_dim_sizes(1), num_count / cur_out_dim_sizes(1) /))
                                    
                                    data_blobs(cur_out_var_ind)%long_2d_buffer &
                                        (1 : cur_out_dim_sizes(1), &
                                            data_blobs(cur_out_var_ind)%cur_pos : &
                                            data_blobs(cur_out_var_ind)%cur_pos + (num_count / cur_out_dim_sizes(1)) - 1) &
                                        = long_2d_buffer(:,:)
                                    
                                    !deallocate(long_buffer)
                                    deallocate(long_2d_buffer)
                                else if (cur_out_var_type == NF90_FLOAT) then
                                    allocate(rsingle_2d_buffer(cur_out_dim_sizes(1), num_count / cur_out_dim_sizes(1)))
                                    !allocate(rsingle_buffer   (num_count))
                                    rsingle_2d_buffer = NF90_FILL_FLOAT
                                    
                                    ! Args: the target variable, number of elements to recv,
                                    ! data type (in MPI land), source process #,
                                    ! numeric tag for extra info, and communicator.
                                    call MPI_Recv(rsingle_2d_buffer, num_count, MPI_FLOAT, &
                                        i_proc, cur_out_var_ind, MPI_COMM_WORLD, mpi_status, ierr)
                                    
                                    !rsingle_2d_buffer = reshape(rsingle_buffer, (/ cur_out_dim_sizes(1), num_count / cur_out_dim_sizes(1) /))
                                    
                                    data_blobs(cur_out_var_ind)%rsingle_2d_buffer &
                                        (1 : cur_out_dim_sizes(1), &
                                            data_blobs(cur_out_var_ind)%cur_pos : &
                                            data_blobs(cur_out_var_ind)%cur_pos + (num_count / cur_out_dim_sizes(1)) - 1) &
                                        = rsingle_2d_buffer(:,:)
                                    
                                    !deallocate(rsingle_buffer)
                                    deallocate(rsingle_2d_buffer)
                                else if (cur_out_var_type == NF90_DOUBLE) then
                                    allocate(rdouble_2d_buffer(cur_out_dim_sizes(1), num_count / cur_out_dim_sizes(1)))
                                    !allocate(rdouble_buffer   (num_count))
                                    rdouble_2d_buffer = NF90_FILL_DOUBLE
                                    
                                    ! Args: the target variable, number of elements to recv,
                                    ! data type (in MPI land), source process #,
                                    ! numeric tag for extra info, and communicator.
                                    call MPI_Recv(rdouble_2d_buffer, num_count, MPI_DOUBLE, &
                                        i_proc, cur_out_var_ind, MPI_COMM_WORLD, mpi_status, ierr)
                                    
                                    !rdouble_2d_buffer = reshape(rdouble_buffer, (/ cur_out_dim_sizes(1), num_count / cur_out_dim_sizes(1) /))
                                    
                                    data_blobs(cur_out_var_ind)%rdouble_2d_buffer &
                                        (1 : cur_out_dim_sizes(1), &
                                            data_blobs(cur_out_var_ind)%cur_pos : &
                                            data_blobs(cur_out_var_ind)%cur_pos + (num_count / cur_out_dim_sizes(1)) - 1) &
                                        = rdouble_2d_buffer(:,:)
                                    
                                    !deallocate(rdouble_buffer)
                                    deallocate(rdouble_2d_buffer)
                                else if (cur_out_var_type == NF90_CHAR) then
                                    allocate(string_2d_buffer (cur_out_dim_sizes(1), cur_out_dim_sizes(2), &
                                        num_count / (cur_out_dim_sizes(1) * cur_out_dim_sizes(2))))
                                    
                                    !! Same again, this time just multiplying...
                                    !allocate(string_1d_buffer (cur_out_dim_sizes(1)* cur_out_dim_sizes(2)* &
                                    !    (num_count / (cur_out_dim_sizes(1) * cur_out_dim_sizes(2)))))
                                    
                                    string_2d_buffer = NF90_FILL_CHAR
                                    
                                    ! Args: the target variable, number of elements to recv,
                                    ! data type (in MPI land), source process #,
                                    ! numeric tag for extra info, and communicator.
                                    call MPI_Recv(string_2d_buffer, num_count, MPI_BYTE, &
                                        i_proc, cur_out_var_ind, MPI_COMM_WORLD, mpi_status, ierr)
                                    
                                    !string_2d_buffer = reshape(string_1d_buffer, (/ cur_out_dim_sizes(1), cur_out_dim_sizes(2), &
                                    !    num_count / (cur_out_dim_sizes(1) * cur_out_dim_sizes(2)) /))
                                    
                                    data_blobs(cur_out_var_ind)%string_2d_buffer &
                                        (1 : cur_out_dim_sizes(1), &
                                            1 : cur_out_dim_sizes(2), &
                                            data_blobs(cur_out_var_ind)%cur_pos : &
                                            data_blobs(cur_out_var_ind)%cur_pos + &
                                                (num_count / (cur_out_dim_sizes(1) * cur_out_dim_sizes(2))) - 1) &
                                        = string_2d_buffer(:,:,:)
                                    
                                    !data_blobs(cur_out_var_ind)%string_2d_buffer(data_blobs(cur_out_var_ind)%cur_pos:,:,:) &
                                    !    = string_2d_buffer(:,:,:)
                                    
                                    !data_blobs(cur_out_var_ind)%string_2d_buffer &
                                    !    (1 : cur_dim_sizes(1), 1 : cur_dim_sizes(2), &
                                    !        data_blobs(cur_out_var_ind)%cur_pos : &
                                    !        data_blobs(cur_out_var_ind)%cur_pos + cur_dim_sizes(3) - 1) & &
                                    !    = string_2d_buffer(:,:,:)
                                    
                                    !call check(nf90_put_var(ncid_output, cur_out_var_id, string_2d_buffer, &
                                    !    start = (/ 1, 1, 1 + dim_counters(nc_diag_cat_lookup_dim(tmp_var_dim_names(3))) /), &
                                    !    count = (/ cur_dim_sizes(1), cur_dim_sizes(2), cur_dim_sizes(3) /) ))
                                    !deallocate(string_1d_buffer)
                                    deallocate(string_2d_buffer)
                                else
                                    write (err_string, "(A, I0, A)") &
                                        "Invalid type detected during write." // &
                                        CHAR(10) // "             " // &
                                        "(Variable '" // trim(tmp_var_name) // "' has an type of ", &
                                        cur_out_var_type, "," // &
                                        CHAR(10) // "             " // &
                                        "which is invalid!)"
                                    call error(trim(err_string))
                                end if
                                
                                if (any(cur_out_dim_sizes == -1)) then
                                    if (cur_out_var_type == NF90_CHAR) then
                                        data_blobs(cur_out_var_ind)%cur_pos = &
                                            data_blobs(cur_out_var_ind)%cur_pos + &
                                            (num_count / (cur_out_dim_sizes(1) * cur_out_dim_sizes(2)))
                                    else
                                        data_blobs(cur_out_var_ind)%cur_pos = &
                                            data_blobs(cur_out_var_ind)%cur_pos + &
                                            (num_count / cur_out_dim_sizes(1))
                                    end if
                                end if
                            end if
                            
                            !print *, "cur_out_var_ndims", cur_out_var_ndims
                            !print *, "cur_dim_sizes(cur_out_var_ndims)", cur_dim_sizes(cur_out_var_ndims)
                            !print *, "any(cur_out_dim_sizes == -1)", any(cur_out_dim_sizes == -1)
                            
                            
                            
                            !var_counters(cur_out_var_ind) = &
                            !        var_counters(cur_out_var_ind) + 1
                            
                            
                        end if
                        
                        !print *, "POGO POST"
                        
                        ! Don't increment until we read the first round of files
                        if (((.NOT. any(cur_out_dim_sizes == -1)) .AND. &
                                (read_var_count(cur_out_var_ind) > (num_procs - 1))) &
                            .OR. (any(cur_out_dim_sizes == -1))) &
                            var_counters(cur_out_var_ind) = &
                                var_counters(cur_out_var_ind) + 1
                        
                        !write (*, "(A, I0)") "Var " // trim(var_names(cur_out_var_ind)) &
                        !    // " | read_var_count(cur_out_var_ind)=",read_var_count(cur_out_var_ind)
                        !write (*, "(A, I0)") "Var " // trim(var_names(cur_out_var_ind)) &
                        !    // " | data_blobs(cur_out_var_ind)%cur_pos=",data_blobs(cur_out_var_ind)%cur_pos
                        
                        read_var_count(cur_out_var_ind) = read_var_count(cur_out_var_ind) + 1
                        
                        deallocate(cur_out_dim_sizes)
                    end do
                end do
                
                ! Attempt to flush the MPI queue!
                ! We need to make sure everything exited properly...
                do i = 1, 1000
                    if (procs_done == (num_procs - 1)) then
                        exit
                    end if
                    do i_proc = 1, num_procs - 1
                        call MPI_Iprobe(i_proc, MPI_ANY_TAG, MPI_COMM_WORLD, mpi_read_flag, mpi_status, ierr)
                        
                        ! Skip the rest if we can't do anything!
                        if (.NOT. mpi_read_flag) &
                            cycle
                        
                        if (ierr /= 0) &
                            call error("MPI ERROR OCCURRED!")
                        
                        ! Finished file tag
                        if (mpi_status(MPI_TAG) == var_arr_total + 1000) then
                            call error("Inconsistency error - getting file completion after" &
                                // char(10) &
                                // "             main data loop end. BUG!")
                        else if (mpi_status(MPI_TAG) == var_arr_total + 2000) then
                            ! Finished process tag
                            
                            !write (*, "(A, I0)") "Process done notification from proc ", i_proc
                            call MPI_Recv(i, 1, MPI_INT, &
                                          i_proc, var_arr_total + 2000, MPI_COMM_WORLD, mpi_status, ierr)
                            procs_done = procs_done + 1
                            procs_done_arr(procs_done) = i_proc
                            cycle
                        else
                            ! We got data... that's really bad!
                            ! This is a bug and we need to exit, ASAP.
                            call error("Inconsistency error - getting variable data after" &
                                // char(10) &
                                // "             main data loop end. BUG!")
                        end if
                    end do
                end do
                
                if (procs_done /= (num_procs - 1)) then
                    call error("Inconsistency error - not all processes completed" &
                        // char(10) &
                        // "             before main data loop end. BUG!")
                end if
                
                !print *, "FINAL FILE COUNT:"
                !print *, file_count
                !print *, "FINAL INPUT COUNT:"
                !print *, input_count
                
                deallocate(read_var_count)
            end if
        end subroutine nc_diag_cat_data_pass
        
        subroutine nc_diag_cat_data_commit
            call info("Doing final data commit...")
            
            do var_index = 1, var_arr_total
                call info(" => Writing variable " // trim(var_names(var_index)) // "...")
                if ((var_dim_names(var_index)%num_names == 1) .OR. &
                    ((var_dim_names(var_index)%num_names == 2) .AND. (var_types(var_index) == NF90_CHAR)) ) then
                    if (var_types(var_index) == NF90_BYTE) &
                        call check(nf90_put_var(ncid_output, var_output_ids(var_index), &
                            data_blobs(var_index)%byte_buffer, &
                            start = (/ 1 /), &
                            count = (/ data_blobs(var_index)%alloc_size(1) /) ))
                    if (var_types(var_index) == NF90_SHORT) &
                        call check(nf90_put_var(ncid_output, var_output_ids(var_index), &
                            data_blobs(var_index)%short_buffer, &
                            start = (/ 1 /), &
                            count = (/ data_blobs(var_index)%alloc_size(1) /) ))
                    if (var_types(var_index) == NF90_INT) &
                        call check(nf90_put_var(ncid_output, var_output_ids(var_index), &
                            data_blobs(var_index)%long_buffer, &
                            start = (/ 1 /), &
                            count = (/ data_blobs(var_index)%alloc_size(1) /) ))
                    if (var_types(var_index) == NF90_FLOAT) &
                        call check(nf90_put_var(ncid_output, var_output_ids(var_index), &
                            data_blobs(var_index)%rsingle_buffer, &
                            start = (/ 1 /), &
                            count = (/ data_blobs(var_index)%alloc_size(1) /) ))
                    
                    if (var_types(var_index) == NF90_DOUBLE) &
                        call check(nf90_put_var(ncid_output, var_output_ids(var_index), &
                            data_blobs(var_index)%rdouble_buffer, &
                            start = (/ 1 /), &
                            count = (/ data_blobs(var_index)%alloc_size(1) /) ))
                    if (var_types(var_index) == NF90_CHAR) &
                        call check(nf90_put_var(ncid_output, var_output_ids(var_index), &
                            data_blobs(var_index)%string_buffer, &
                            start = (/ 1, 1 /), &
                            count = (/ data_blobs(var_index)%alloc_size(1), &
                                data_blobs(var_index)%alloc_size(2) /) ))
                else if ((var_dim_names(var_index)%num_names == 2) .OR. &
                    ((var_dim_names(var_index)%num_names == 3) .AND. (var_types(var_index) == NF90_CHAR)) ) then
                    if (var_types(var_index) == NF90_BYTE) &
                        call check(nf90_put_var(ncid_output, var_output_ids(var_index), &
                            data_blobs(var_index)%byte_2d_buffer, &
                            start = (/ 1, 1 /), &
                            count = (/ data_blobs(var_index)%alloc_size(1), &
                                data_blobs(var_index)%alloc_size(2) /) ))
                    if (var_types(var_index) == NF90_SHORT) &
                        call check(nf90_put_var(ncid_output, var_output_ids(var_index), &
                            data_blobs(var_index)%short_2d_buffer, &
                            start = (/ 1, 1 /), &
                            count = (/ data_blobs(var_index)%alloc_size(1), &
                                data_blobs(var_index)%alloc_size(2) /) ))
                    if (var_types(var_index) == NF90_INT) &
                        call check(nf90_put_var(ncid_output, var_output_ids(var_index), &
                            data_blobs(var_index)%long_2d_buffer, &
                            start = (/ 1, 1 /), &
                            count = (/ data_blobs(var_index)%alloc_size(1), &
                                data_blobs(var_index)%alloc_size(2) /) ))
                    if (var_types(var_index) == NF90_FLOAT) &
                        call check(nf90_put_var(ncid_output, var_output_ids(var_index), &
                            data_blobs(var_index)%rsingle_2d_buffer, &
                            start = (/ 1, 1 /), &
                            count = (/ data_blobs(var_index)%alloc_size(1), &
                                data_blobs(var_index)%alloc_size(2) /) ))
                    if (var_types(var_index) == NF90_DOUBLE) &
                        call check(nf90_put_var(ncid_output, var_output_ids(var_index), &
                            data_blobs(var_index)%rdouble_2d_buffer, &
                            start = (/ 1, 1 /), &
                            count = (/ data_blobs(var_index)%alloc_size(1), &
                                data_blobs(var_index)%alloc_size(2) /) ))
                    if (var_types(var_index) == NF90_CHAR) &
                        call check(nf90_put_var(ncid_output, var_output_ids(var_index), &
                            data_blobs(var_index)%string_2d_buffer, &
                            start = (/ 1, 1, 1 /), &
                            count = (/ data_blobs(var_index)%alloc_size(1), &
                                data_blobs(var_index)%alloc_size(2), &
                                data_blobs(var_index)%alloc_size(3) /) ))
                end if
            end do
        end subroutine nc_diag_cat_data_commit


