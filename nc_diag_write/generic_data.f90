program generic_data

  ! dimensions
  integer   :: num_record  = 5
  integer   :: num_chan    = 3

  ! header variables

  character*20 :: instrument, satellite
  integer      :: analysis_date

  ! arrays that will have data to write - note latitude and longitude are 1D 
  real,dimension(:),allocatable      :: latitude, longitude
  real,dimension(:,:),allocatable    :: temperature, error

  integer,dimension(:,:),allocatable :: channel 

  integer :: iob, ichan
  integer :: current_entry

  allocate( latitude(num_record),  &
            longitude(num_record) )

  allocate( temperature(num_chan, num_record),  &
            error(num_chan, num_record),        &
            channel(num_chan, num_record)       )

  ! set header variables
  instrument = "geninst"
  satellite  = "gsat"

  analysis_date = 2012122500 ! YYYYMMDDHH

  print *,'--------'
  print *,'HEADER: '
  print *,'--------'

  write(*,fmt='(6X,"Instrument:",1X,A20)')instrument
  write(*,fmt='(6X,"Satellite:",1X,A20)')satellite
  write(*,fmt='(6X,"Date:",1X,I10)')analysis_date

  latitude(1) = -60.0 ; longitude(1) = 120.0 ; temperature(1,1) = 221.0 ; temperature(2,1) = 241.0 ; temperature(3,1) = 261.0
                                                     error(1,1) =   1.1 ;       error(2,1) =   2.1 ;       error(3,1) =   3.1
                                                   channel(1,1) =     1 ;     channel(2,1) =     2 ;     channel(3,1) =     3

  latitude(2) = -30.0 ; longitude(2) =  90.0 ; temperature(1,2) = 222.0 ; temperature(2,2) = 242.0 ; temperature(3,2) = 262.0
                                                     error(1,2) =   1.2 ;       error(2,2) =   2.2 ;       error(3,2) =   3.2
                                                   channel(1,2) =     1 ;     channel(2,2) =     2 ;     channel(3,2) =     3

  latitude(3) =   0.0 ; longitude(3) =  60.0 ; temperature(1,3) = 223.0 ; temperature(2,3) = 243.0 ; temperature(3,3) = 263.0
                                                     error(1,3) =   1.3 ;       error(2,3) =   2.3 ;       error(3,3) =   3.3
                                                   channel(1,3) =     1 ;     channel(2,3) =     2 ;     channel(3,3) =     3

  latitude(4) =  30.0 ; longitude(4) =  30.0 ; temperature(1,4) = 224.0 ; temperature(2,4) = 244.0 ; temperature(3,4) = 264.0
                                                     error(1,4) =   1.4 ;       error(2,4) =   2.4 ;       error(3,4) =   3.4
                                                   channel(1,4) =     1 ;     channel(2,4) =     2 ;     channel(3,4) =     3

  latitude(5) =  60.0 ; longitude(5) =   0.0 ; temperature(1,5) = 225.0 ; temperature(2,5) = 245.0 ; temperature(3,5) = 265.0
                                                     error(1,5) =   1.5 ;       error(2,5) =   2.5 ;       error(3,5) =   3.5
                                                   channel(1,5) =     1 ;     channel(2,5) =     2 ;     channel(3,5) =     3
 

  current_entry = 0

  print *, "---------------"
  print *, "In datum space:"
  print *, "---------------"

  do iob=1,num_record
    do ichan=1,num_chan
      current_entry = current_entry + 1     
      write(*,fmt='(6X,"Datum:",I4,1X,"Latitude:",F8.2,1X,"Longitude:",F8.2,1X,"Channel:",I4,1X,"Temperature:",F8.2,1X,"Error:",F8.2)'), &
                                                      current_entry,latitude(iob),longitude(iob),channel(ichan,iob),temperature(ichan,iob),error(ichan,iob)
    enddo
  enddo

  print *, "----------------"
  print *, "In record space:"
  print *, "----------------"

  current_entry = 0

  do iob=1,num_record
    current_entry = current_entry + 1
    write(*,fmt='(6X,"Record:",I4,1X,"Latitude:",F8.2,1X,"Longitude:",F8.2)') current_entry,latitude(iob),longitude(iob)
    do ichan=1,num_chan
      write(*,fmt='(12X,"Channel:",I4,1X,"Temperature:",F8.2,1X,"Error:",F8.2)'), &
                                                      channel(ichan,iob),temperature(ichan,iob),error(ichan,iob)
    enddo
  enddo

end  
    
