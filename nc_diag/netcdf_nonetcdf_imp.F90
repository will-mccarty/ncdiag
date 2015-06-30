#include "netcdf_nonetcdf_putvar.F90"
        
        function nf90_strerror(ncerr)
            integer, intent( in) :: ncerr
            character(len = 80)  :: nf90_strerror
            
            nf90_strerror = "NetCDF disabled for debugging purposes."
        end function nf90_strerror
        
        function nf90_def_dim(ncid, name, len, dimid)
            integer,             intent( in) :: ncid
            character (len = *), intent( in) :: name
            integer,             intent( in) :: len
            integer,             intent(out) :: dimid
            integer                          :: nf90_def_dim
        
            nf90_def_dim = 0
            dimid = 0
        end function nf90_def_dim
        
        function nf90_def_var_Scalar(ncid, name, xtype, varid)
            integer,               intent( in) :: ncid
            character (len = *),   intent( in) :: name
            integer,               intent( in) :: xtype
            integer,               intent(out) :: varid
            integer                            :: nf90_def_var_Scalar
            
            ! Dummy - shouldn't get used
            integer, dimension(1) :: dimids
            
            nf90_def_var_Scalar = 0
        end function nf90_def_var_Scalar
        ! ----- 
        function nf90_def_var_oneDim(ncid, name, xtype, dimids, varid)
            integer,               intent( in) :: ncid
            character (len = *),   intent( in) :: name
            integer,               intent( in) :: xtype
            integer,               intent( in) :: dimids
            integer,               intent(out) :: varid
            integer                            :: nf90_def_var_oneDim
            
            integer, dimension(1) :: dimidsA
            dimidsA(1) = dimids
            nf90_def_var_oneDim = 0
        end function nf90_def_var_oneDim
        ! ----- 
        function nf90_def_var_ManyDims(ncid, name, xtype, dimids, varid)
            integer,               intent( in) :: ncid
            character (len = *),   intent( in) :: name
            integer,               intent( in) :: xtype
            integer, dimension(:), intent( in) :: dimids
            integer,               intent(out) :: varid
            integer                            :: nf90_def_var_ManyDims
            
            nf90_def_var_ManyDims = 0
        end function nf90_def_var_ManyDims
        
        function nf90_put_att_text(ncid, varid, name, values)
            integer,                          intent( in) :: ncid, varid
            character(len = *),               intent( in) :: name
            character(len = *),               intent( in) :: values
            integer                                       :: nf90_put_att_text
        
            nf90_put_att_text = 0
        end function nf90_put_att_text
        ! -------
        function nf90_get_att_text(ncid, varid, name, values)
            integer,                          intent( in) :: ncid, varid
            character(len = *),               intent( in) :: name
            character(len = *),               intent(out) :: values
            integer                                       :: nf90_get_att_text
        
            nf90_get_att_text = 0
        end function nf90_get_att_text
        ! -------
        ! Integer attributes
        ! -------
        function nf90_put_att_OneByteInt(ncid, varid, name, values)
            integer,                                    intent( in) :: ncid, varid
            character(len = *),                         intent( in) :: name
            integer (kind =  i_byte), dimension(:), intent( in) :: values
            integer                                                 :: nf90_put_att_OneByteInt
        
            nf90_put_att_OneByteInt = 0
        end function nf90_put_att_OneByteInt
        ! -------
        function nf90_put_att_one_OneByteInt(ncid, varid, name, values)
            integer,                                    intent( in) :: ncid, varid
            character(len = *),                         intent( in) :: name
            integer (kind =  i_byte),               intent( in) :: values
            integer                                                 :: nf90_put_att_one_OneByteInt
            
            nf90_put_att_one_OneByteInt = 0
        end function nf90_put_att_one_OneByteInt
        ! -------
        function nf90_get_att_OneByteInt(ncid, varid, name, values)
            integer,                                    intent( in) :: ncid, varid
            character(len = *),                         intent( in) :: name
            integer (kind =  i_byte), dimension(:), intent(out) :: values
            integer                                                 :: nf90_get_att_OneByteInt
        
            nf90_get_att_OneByteInt = 0
        end function nf90_get_att_OneByteInt
        ! -------
        function nf90_get_att_one_OneByteInt(ncid, varid, name, values)
            integer,                                    intent( in) :: ncid, varid
            character(len = *),                         intent( in) :: name
            integer (kind =  i_byte),               intent(out) :: values
            integer                                                 :: nf90_get_att_one_OneByteInt
            
            nf90_get_att_one_OneByteInt = 0
        end function nf90_get_att_one_OneByteInt
        ! -------
        function nf90_put_att_TwoByteInt(ncid, varid, name, values)
            integer,                                    intent( in) :: ncid, varid
            character(len = *),                         intent( in) :: name
            integer (kind =  i_short), dimension(:), intent( in) :: values
            integer                                                 :: nf90_put_att_TwoByteInt
        
            nf90_put_att_TwoByteInt = 0
        end function nf90_put_att_TwoByteInt
        ! -------
        function nf90_put_att_one_TwoByteInt(ncid, varid, name, values)
            integer,                                    intent( in) :: ncid, varid
            character(len = *),                         intent( in) :: name
            integer (kind =  i_short),               intent( in) :: values
            integer                                                 :: nf90_put_att_one_TwoByteInt
            
            nf90_put_att_one_TwoByteInt = 0
        end function nf90_put_att_one_TwoByteInt
        ! -------
        function nf90_get_att_TwoByteInt(ncid, varid, name, values)
            integer,                                    intent( in) :: ncid, varid
            character(len = *),                         intent( in) :: name
            integer (kind =  i_short), dimension(:), intent(out) :: values
            integer                                                 :: nf90_get_att_TwoByteInt
        
            nf90_get_att_TwoByteInt = 0
        end function nf90_get_att_TwoByteInt
        ! -------
        function nf90_get_att_one_TwoByteInt(ncid, varid, name, values)
            integer,                                    intent( in) :: ncid, varid
            character(len = *),                         intent( in) :: name
            integer (kind =  i_short),               intent(out) :: values
            integer                                                 :: nf90_get_att_one_TwoByteInt
            
            nf90_get_att_one_TwoByteInt = 0
        end function nf90_get_att_one_TwoByteInt
        ! -------
        function nf90_put_att_FourByteInt(ncid, varid, name, values)
            integer,                                    intent( in) :: ncid, varid
            character(len = *),                         intent( in) :: name
            integer (kind = i_long), dimension(:), intent( in) :: values
            integer                                                 :: nf90_put_att_FourByteInt
        
            nf90_put_att_FourByteInt = 0
        end function nf90_put_att_FourByteInt
        ! -------
        function nf90_put_att_one_FourByteInt(ncid, varid, name, values)
            integer,                                    intent( in) :: ncid, varid
            character(len = *),                         intent( in) :: name
            integer (kind = i_long),               intent( in) :: values
            integer                                                 :: nf90_put_att_one_FourByteInt
            
            nf90_put_att_one_FourByteInt = 0
        end function nf90_put_att_one_FourByteInt
        ! -------
        function nf90_get_att_FourByteInt(ncid, varid, name, values)
            integer,                                    intent( in) :: ncid, varid
            character(len = *),                         intent( in) :: name
            integer (kind = i_long), dimension(:), intent(out) :: values
            integer                                                 :: nf90_get_att_FourByteInt
            
            nf90_get_att_FourByteInt = 0
        end function nf90_get_att_FourByteInt
        ! -------
        function nf90_get_att_one_FourByteInt(ncid, varid, name, values)
            integer,                                    intent( in) :: ncid, varid
            character(len = *),                         intent( in) :: name
            integer (kind = i_long),               intent(out) :: values
            integer                                                 :: nf90_get_att_one_FourByteInt
        
            integer, dimension(1) :: defaultInteger
        
            nf90_get_att_one_FourByteInt = 0
        end function nf90_get_att_one_FourByteInt
        ! -------
        function nf90_put_att_EightByteInt(ncid, varid, name, values)
            integer,                                     intent( in) :: ncid, varid
            character(len = *),                          intent( in) :: name
            integer (kind = i_llong), dimension(:), intent( in) :: values
            integer                                                  :: nf90_put_att_EightByteInt
        
            nf90_put_att_EightByteInt = 0
        end function nf90_put_att_EightByteInt
        ! -------
        function nf90_put_att_one_EightByteInt(ncid, varid, name, values)
            integer,                                     intent( in) :: ncid, varid
            character(len = *),                          intent( in) :: name
            integer (kind = i_llong),               intent( in) :: values
            integer                                                  :: nf90_put_att_one_EightByteInt
            
            nf90_put_att_one_EightByteInt = 0
        end function nf90_put_att_one_EightByteInt
        ! -------
        function nf90_get_att_EightByteInt(ncid, varid, name, values)
            integer,                                     intent( in) :: ncid, varid
            character(len = *),                          intent( in) :: name
            integer (kind = i_llong), dimension(:), intent(out) :: values
            integer                                                 :: nf90_get_att_EightByteInt
            
            nf90_get_att_EightByteInt = 0
        end function nf90_get_att_EightByteInt
        ! -------
        function nf90_get_att_one_EightByteInt(ncid, varid, name, values)
            integer,                                     intent( in) :: ncid, varid
            character(len = *),                          intent( in) :: name
            integer (kind = i_llong),               intent(out) :: values
            integer                                                 :: nf90_get_att_one_EightByteInt
            
            nf90_get_att_one_EightByteInt = 0
        end function nf90_get_att_one_EightByteInt
        ! -------
        ! Real attributes
        ! -------
        function nf90_put_att_FourByteReal(ncid, varid, name, values)
            integer,                                   intent( in) :: ncid, varid
            character(len = *),                        intent( in) :: name
            real (kind =  r_single), dimension(:), intent( in) :: values
            integer                                                :: nf90_put_att_FourByteReal
        
            nf90_put_att_FourByteReal = 0
        end function nf90_put_att_FourByteReal
        ! -------
        function nf90_put_att_one_FourByteReal(ncid, varid, name, values)
            integer,                                   intent( in) :: ncid, varid
            character(len = *),                        intent( in) :: name
            real (kind =  r_single),               intent( in) :: values
            integer                                                :: nf90_put_att_one_FourByteReal
            
            nf90_put_att_one_FourByteReal = 0
        end function nf90_put_att_one_FourByteReal
        ! -------
        function nf90_get_att_FourByteReal(ncid, varid, name, values)
            integer,                                   intent( in) :: ncid, varid
            character(len = *),                        intent( in) :: name
            real (kind =  r_single), dimension(:), intent(out) :: values
            integer                                                :: nf90_get_att_FourByteReal
        
            nf90_get_att_FourByteReal = 0
        end function nf90_get_att_FourByteReal
        ! -------
        function nf90_get_att_one_FourByteReal(ncid, varid, name, values)
            integer,                                   intent( in) :: ncid, varid
            character(len = *),                        intent( in) :: name
            real (kind =  r_single),               intent(out) :: values
            integer                                                :: nf90_get_att_one_FourByteReal
            
            nf90_get_att_one_FourByteReal = 0
        end function nf90_get_att_one_FourByteReal
        ! -------
        function nf90_put_att_EightByteReal(ncid, varid, name, values)
            integer,                                   intent( in) :: ncid, varid
            character(len = *),                        intent( in) :: name
            real (kind = r_double), dimension(:), intent( in) :: values
            integer                                                :: nf90_put_att_EightByteReal
        
            nf90_put_att_EightByteReal = 0
        end function nf90_put_att_EightByteReal
        ! -------
        function nf90_put_att_one_EightByteReal(ncid, varid, name, values)
            integer,                                   intent( in) :: ncid, varid
            character(len = *),                        intent( in) :: name
            real (kind = r_double),               intent( in) :: values
            integer                                                :: nf90_put_att_one_EightByteReal
            
            nf90_put_att_one_EightByteReal = 0
        end function nf90_put_att_one_EightByteReal
        ! -------
        function nf90_get_att_EightByteReal(ncid, varid, name, values)
            integer,                                   intent( in) :: ncid, varid
            character(len = *),                        intent( in) :: name
            real (kind = r_double), dimension(:), intent(out) :: values
            integer                                                :: nf90_get_att_EightByteReal
        
            nf90_get_att_EightByteReal = 0
        end function nf90_get_att_EightByteReal
        ! -------
        function nf90_get_att_one_EightByteReal(ncid, varid, name, values)
            integer,                                   intent( in) :: ncid, varid
            character(len = *),                        intent( in) :: name
            real (kind = r_double),               intent(out) :: values
            integer                                                :: nf90_get_att_one_EightByteReal
            
            nf90_get_att_one_EightByteReal = 0
        end function nf90_get_att_one_EightByteReal
        
        function nf90_def_var_chunking(ncid, varid, contiguous, chunksizes)
            integer, intent(in) :: ncid
            integer, intent(in) :: varid
            integer, intent(in) :: contiguous
            integer, dimension(:), intent(in) :: chunksizes
            integer :: nf90_def_var_chunking
            
            nf90_def_var_chunking = 0
        end function nf90_def_var_chunking
        
        function nf90_def_var_deflate(ncid, varid, shuffle, deflate, deflate_level)
            integer, intent(in) :: ncid
            integer, intent(in) :: varid
            integer, intent(in) :: shuffle
            integer, intent(in) :: deflate
            integer, intent(in) :: deflate_level
            integer :: nf90_def_var_deflate
            
            nf90_def_var_deflate = 0
        end function nf90_def_var_deflate
