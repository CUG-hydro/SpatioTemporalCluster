!**********************************************************************
!  MODULE InputOutput                                                 *
!  PURPOSE     Input / output subroutines                             *
!  CREATED     Luis Samaniego, 09.02.2011                             *
!  MODIFIED    Stephan Thober, 07.11.2017 - switched to mo_netcdf     *
!                                           added invert_SMI          *
!**********************************************************************
MODULE mo_read 

  USE mo_kind,   only : i4, sp, dp

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: ReadDataMain   ! read everything

  ! ------------------------------------------------------------------

CONTAINS

  ! ------------------------------------------------------------------


  !*********************************************************************
  !    SUBROUTINE Read Database
  !    PURPOSE    Reads main file
  !    AUTHOR:    Luis E. Samaniego-Eguiguren, UFZ 23.05.2007
  !    NOTES:
  !               packed fields are stored in dim1->dim2 sequence 
  !*********************************************************************
  subroutine ReadDataMain( SMI_in, do_cluster, ext_smi, invert_SMI, read_opt_h, silverman_h, opt_h, lats, lons,  &
                           basin_flag, mask, SM_kde,  SM_eval,  &
                           Basin_Id, SMI_thld, outpath, cellsize, thCellClus, nCellInter, do_sad, deltaArea, &
                           nCalendarStepsYear, per_kde, per_eval, per_smi )

    use mo_kind,             only: i4
    use mo_message,          only: message
    use mo_utils,            only: notequal, equal
    use mo_netcdf,           only: NcDataset, NcVariable
    use mo_smi_constants,    only: nodata_dp
    use mo_global_variables, only: period
    
    implicit none

    ! input / output Variables
    real(sp),    dimension(:,:), allocatable, intent(out) :: SMI_in      ! SMI only read if ext_smi is TRUE
    logical,                                  intent(out) :: do_sad      ! do SAD analysis
    logical,                                  intent(out) :: do_cluster  ! do cluster calculation
    logical,                                  intent(out) :: ext_smi ! clsutering external data
    logical,                                  intent(out) :: invert_SMI  ! do inversion of SMI
    logical,                                  intent(out) :: read_opt_h  ! read kernel width
    logical,                                  intent(out) :: silverman_h ! optimize kernel width
    logical,                                  intent(out) :: basin_flag  ! basin flag
    logical,     dimension(:,:), allocatable, intent(out) :: mask        ! grid mask
    integer(i4),                              intent(out) :: thCellClus  ! treshold  for cluster formation in space ~ 640 km2
    integer(i4),                              intent(out) :: nCellInter  ! number cells for joining clusters in time ~ 6400 km2
    integer(i4),                              intent(out) :: deltaArea   ! number of cells per area interval
    integer(i4),                              intent(out) :: nCalendarStepsYear  ! number of calendar time steps per
                                                                                 ! year (month=12, day=365)
    
    integer(i4), dimension(:,:), allocatable, intent(out) :: Basin_Id    ! IDs for basinwise drought analysis
    real(sp),                                 intent(out) :: cellsize    ! cell edge lenght of input data
    real(sp),    dimension(:,:), allocatable, intent(out) :: SM_kde      ! daily / monthly fields packed for estimation
    real(sp),    dimension(:,:), allocatable, intent(out) :: SM_eval     ! monthly fields packed for evaluation
    real(dp),    dimension(:,:), allocatable, intent(out) :: opt_h       ! optimized kernel width
    real(dp),    dimension(:,:), allocatable, intent(out) :: lats, lons  ! latitude and longitude fields of input
    real(sp),                                 intent(out) :: SMI_thld    ! SMI threshold for clustering
    character(len=256),                       intent(out) :: outpath     ! ouutput path for results
    type(period),                             intent(out) :: per_kde     ! period contain start and end date information during estimation
    type(period),                             intent(out) :: per_eval    ! period during evaluation 
    type(period),                             intent(out) :: per_smi     ! period of smi file

    ! local Variables
    logical                                         :: read_sm
    integer(i4)                                     :: ii
    integer(i4)                                     :: nCells         ! number of effective cells
    integer(i4)                                     :: lag! number of lag days for daily files (0,7,31)
    integer(i4)                                     :: nTimeSteps     ! number of time steps excluding leap days
    logical                                         :: file_exist
    
    ! directories, filenames, attributes
    character(256)                                  :: maskfName
    character(256)                                  :: opt_h_file
    character(256)                                  :: basinfName
    
    ! variable names in netcdf input files
    character(256)                                  :: soilmoist_file
    character(256)                                  :: mask_vname
    character(256)                                  :: SM_vname
    character(256)                                  :: ext_smi_file
    character(256)                                  :: basin_vname
    character(256)                                  :: opt_h_vname
    character(256)                                  :: opt_h_sm_vname
    real(sp)                                        :: nodata_value ! local data nodata value in nc for mask creation

    real(sp),       dimension(:,:),     allocatable :: dummy_D2_sp
    real(sp),       dimension(:,:,:),   allocatable :: dummy_D3_sp
    real(dp),       dimension(:,:,:),   allocatable :: dummy_D3_dp

    type(NcDataset)                                 :: nc_in
    type(NcVariable)                                :: nc_var

    ! read main config
    namelist/mainconfig/basin_flag, basinfName, basin_vname, maskfName, mask_vname, &
         soilmoist_file, SM_vname, outpath, nCalendarStepsYear, lag, &
         silverman_h, read_opt_h, opt_h_vname, opt_h_sm_vname, opt_h_file,     &
         SMI_thld, do_cluster, ext_smi, ext_smi_file, cellsize, thCellClus, nCellInter, &
         do_sad, deltaArea, invert_SMI


    call message("")
    call message("====================================================")
    call message("||                                                ||")
    call message("||              SOIL MOISTURE INDEX               ||")
    call message("||                 VERSION 2.0.1                  ||")
    call message("||                                                ||")
    call message("====================================================")
    call message("")
    
    ! dummy init to avoid gnu compiler complaint
    outpath    = '-999'; cellsize=-999.0_sp; thCellClus=-999; nCellInter=-999; deltaArea=-999

    do_cluster = .FALSE.
    do_sad = .FALSE.
    SMI_thld   = 0.0_dp
    silverman_h = .FALSE.
    
    ! read namelist
    open (unit=10, file='main.dat', status='old')
      read(10, nml=mainconfig)
    close (10)
    call message('main.dat read ...ok')

    ! consistency check
    if ( .not.(( nCalendarStepsYear == 12) .or. ( nCalendarStepsYear == 365) ) ) &
         stop '***ERROR: number of time steps per year different from 12 or 365'

    if ( ( lag .gt. 0 ) .and. ( nCalendarStepsYear == 12)  ) &
         stop '***ERROR: lag = 0 for montly SM values'

    if ( do_sad .and. (.not. do_cluster) ) &
         stop '***ERROR: flag do_cluster must be set to .TRUE. to perform SAD analisys'

    if (invert_SMI .and. (.not. ext_smi)) &
         stop '***ERROR: if invert_SMI is .TRUE., then ext_smi must be .TRUE. and ext_smi_file must be given.'
    
    ! read sm if not external smi or invert is given
    read_sm = (.not. ext_smi)
    
    ! read main mask
    inquire(file=maskfName, exist=file_exist )
    if (file_exist) then
      nc_in  = NcDataset(maskfName, "r")
      nc_var = nc_in%getVariable(trim(mask_vname))
      call nc_var%getData(dummy_D2_sp)
      call nc_var%getAttribute('missing_value', nodata_value)
      call read_latlon(nc_in, lats, lons)
      call nc_in%close()
    
      ! create mask
      mask = merge( .true., .false., notequal(dummy_D2_sp, nodata_value ) )
      deallocate( dummy_D2_sp )
      nCells = n_cells_consistency(mask)
      call message('mask read ...ok')
    else
      call message('Mask file ' // trim(maskfName) // ' does not exist!')
    end if
      
    ! read basin mask
    if ( basin_flag .AND. (.NOT. ext_smi)) then
       nc_in  = NcDataset(basinfName, "r")
       nc_var = nc_in%getVariable(trim(basin_vname))
       call nc_var%getData(Basin_id)
       call nc_var%getAttribute('missing_value', nodata_value)
       call nc_in%close()
       
       ! consistency check
       if ( ( size( Basin_Id, 1) .ne. size( mask, 1 ) ) .or. &
            ( size( Basin_Id, 2) .ne. size( mask, 2 ) ) ) then
          call message('***ERROR: size mismatch between basin field and given mask file')
          stop 1
       end if
       ! intersect mask and basin_id
       mask     = merge( .true., .false., mask .and. ( Basin_Id .ne. int( nodata_value, i4) ) )
       Basin_Id = merge( Basin_Id, int( nodata_value, i4), mask )
       call message('basin ID read ...ok')
    end if

    ! *************************************************************************
    ! >>> read SM field
    ! *************************************************************************
    if ( read_sm )  then
       nc_in  = NcDataset(soilmoist_file, "r")
       nc_var = nc_in%getVariable(trim(SM_vname))
       call nc_var%getData(dummy_D3_sp)
       if (.not. allocated(mask)) then
         call nc_var%getAttribute('missing_value', nodata_value)
         mask = (dummy_D3_sp(:, :, 1) .ne. nodata_value)
         nCells = n_cells_consistency(mask)
         call message('mask read ...ok')
       end if

       ! consistency check
       if ( ( size( dummy_D3_sp, 1) .ne. size( mask, 1 ) ) .or. &
            ( size( dummy_D3_sp, 2) .ne. size( mask, 2 ) ) ) then
          call message('***ERROR: size mismatch between SM field and given mask file')
          call nc_in%close()
          stop 1
       end if

       ! find number of leap years in  SM data set
       ! get timepoints in days and masks for climatologies at calendar day or month 
       call get_time(nc_in,  size( dummy_D3_sp, 3 ), per_eval)
      
       ! read lats and lon from file
       call read_latlon( nc_in, lats, lons)
       call nc_in%close()
       
       
       ! if ( nCalendarStepsYear .eq. YearMonths ) then
       allocate( SM_eval( nCells, size( dummy_D3_sp, 3 ) ) )
       ! no lag average, use monthly data as read
       do ii = 1, size( dummy_D3_sp, 3 )
         SM_eval(:,ii) = pack( real(dummy_D3_sp(:,:,ii),sp), mask )
       end do
  
       deallocate( dummy_D3_sp )

       ! calculate moving-lag average
       if (lag .gt. 0) then
         call message('calculate moving average')
         nTimeSteps = size(SM_eval, dim=2)
         allocate(dummy_d2_sp(size(SM_eval, dim=1), size(SM_eval, dim=2)))
         dummy_d2_sp = SM_eval
         do ii = 1, nTimeSteps
           if ( ii .lt. lag ) then
             SM_eval(:,ii) = dummy_d2_sp(:,ii)
           else
             SM_eval(:,ii) = sum(dummy_D2_sp(:,ii-lag+1:ii), DIM=2) / real(lag,sp)
           end if
         end do
         deallocate(dummy_d2_sp)
       end if
       
    end if

    ! *************************************************************************
    ! >>> read_opt_h
    ! *************************************************************************
    if (allocated(mask)) then
      allocate( opt_h(ncells, nCalendarStepsYear))
      opt_h = nodata_dp
    end if
    !
    if ( read_opt_h ) then

      nc_in  = NcDataset(trim( opt_h_file ), 'r')

      nc_var = nc_in%getVariable(trim(opt_h_sm_vname))
      call nc_var%getData(dummy_D3_sp)
      ! read mask if not done already
      if (.not. allocated(mask)) then
        call nc_var%getAttribute('missing_value', nodata_value)
        mask = (dummy_D3_sp(:, :, 1) .ne. nodata_value)
        nCells = n_cells_consistency(mask)
        allocate( opt_h(ncells, nCalendarStepsYear))
        opt_h = nodata_dp
        call message('mask read ...ok')
      end if
      ! unpack values
      allocate(SM_kde(nCells, size(dummy_D3_sp, 3)))
      do ii = 1, size( dummy_D3_sp, 3 )
        SM_kde(:, ii) = pack( dummy_D3_sp(:,:,ii), mask)
      end do
      deallocate( dummy_D3_sp )
      call message('read kde soil moisture values from file... ok')

      ! read optimized kernel width from file
      nc_var = nc_in%getVariable(trim(opt_h_vname))
      call nc_var%getData(dummy_D3_dp)
      do ii = 1, size( dummy_D3_dp, 3 )
        opt_h( :, ii ) = pack( real( dummy_D3_dp(:,:,ii),sp ), mask )
      end do
      deallocate( dummy_D3_dp )
      if ( any( equal( opt_h, nodata_dp ) ) ) &
      ! if ( any( opt_h .eq. nodata_dp ) ) &
          stop '***ERROR kernel width contains nodata values'
      call message('read kde kernel width from file... ok')


      ! get timepoints in days and mask of months
      call get_time(nc_in, size(SM_kde, 2), per_kde)
      ! close file
      call nc_in%close()

    else
      ! SM_eval is SM_kde too
      if (allocated(SM_eval)) then
        SM_kde = SM_eval
        per_kde = per_eval
      end if
    end if

    if (allocated(SM_kde)) then
      if ((modulo(size( SM_kde, 2 ), nCalendarStepsYear) .ne. per_kde%n_leap_days)) then
#ifdef SMIDEBUG
        print *, modulo(size( SM_kde, 2 ), nCalendarStepsYear),  per_kde%n_leap_days
#endif      
        print *, '***ERROR: timesteps in provided SM_kde field must be multiple of nCalendarStepsYear (', &
            nCalendarStepsYear, '; leap days are accounted for)'
        stop
      end if
    end if


    ! *************************************************************************
    ! >>> read external SMI field
    ! *************************************************************************
    if (ext_smi) then
      nc_in  = NcDataset(trim(ext_smi_file), 'r')
      nc_var = nc_in%getVariable('SMI')
      call nc_var%getData(dummy_D3_sp)
      if (.not. allocated(mask)) then
        call nc_var%getAttribute('missing_value', nodata_value)
        mask = (dummy_D3_sp(:, :, 1) .ne. nodata_value)
        nCells = n_cells_consistency(mask)
        call message('mask read ...ok')
      end if
      ! consistency check
      if ( ( size( dummy_D3_sp, 1) .ne. size( mask, 1 ) ) .or. &
           ( size( dummy_D3_sp, 2) .ne. size( mask, 2 ) ) ) then
         call message('***ERROR: size mismatch between SMI field and given mask file')
         stop
      end if

      allocate(SMI_in( nCells, size( dummy_D3_sp, 3 ) ) )
      do ii = 1, size( dummy_D3_sp, 3 )
         SMI_in(:,ii) = pack(dummy_D3_sp(:,:,ii), mask)
      end do
      deallocate( dummy_D3_sp )

      ! get timepoints in days and mask of months
      call get_time( nc_in, size(SMI_in, dim=2), per_smi)

      call read_latlon( nc_in, lats, lons)
      call nc_in%close()
    else
      per_smi = per_kde
    end if

  end subroutine ReadDataMain


  !
  !     PORPOSE
  !         Convert input time into months & check timesteps & determine mask for months
  
  !     CALLING SEQUENCE
  !         call get_time(fName, vname, timevector, maskvector)
  
  !     RESTRICTIONS
  !         None
  
  !     LITERATURE
  !         None
  
  !     HISTORY
  !         Written,  Matthias Zink, Oct 2012
  !         Modified, Stephan Thober, Nov 2017 - convert to mo_netcdf
  !         Modified, Stephan Thober, Aug 2019 - added type period

  subroutine get_time(nc_in, in_time_steps, per_out) !, mask)
    !
    use mo_julian,           only: date2dec, dec2date
    use mo_message,          only: message
    use mo_string_utils,     only: DIVIDE_STRING
    use mo_netcdf,           only: NcDataset, NcVariable
    use mo_smi_constants,    only: YearMonths, DayHours
    use mo_global_variables, only: period, period_init

    implicit none

    type(NcDataset),              intent(in)  :: nc_in      ! NetCDF dataset
    integer(i4),                  intent(in)  :: in_time_steps
    type(period),                 intent(out) :: per_out
    integer(i4)                               :: yStart     ! start year  of the dataser
    integer(i4)                               :: mStart     ! start month of the dataser
    integer(i4)                               :: dStart     ! start day   of the dataser
    integer(i4)                               :: yEnd       ! end year of the dataset
    integer(i4)                               :: mEnd       ! end month of the dataset
    integer(i4)                               :: dEnd       ! end day of the dataset
    integer(i4)                               :: nTimeSteps ! size of the time dimension
    integer(i4), dimension(:),   allocatable  :: timepoints
    ! timestep in days or months

    type(ncVariable)                          :: nc_var
    integer(i4)                               :: i                      ! loop variable
    integer(i4)                               :: yRef, dRef, mRef       ! reference time of NetCDF (unit attribute of
    integer(i4)                               :: month, year, d         ! 
    !
    integer(i4),   dimension(:), allocatable  :: timesteps              ! time variable of NetCDF in input units
    !
    character(256)                            :: AttValues              ! netcdf attribute values
    character(256), dimension(:), allocatable :: strArr                 ! dummy for netcdf attribute handling
    character(256), dimension(:), allocatable :: date                   ! dummy for netcdf attribute handling
    real(dp)                                  :: ref_jday               ! refernce date of dateset as julian day 
    !
    ! get unit attribute of variable 'time'
    nc_var = nc_in%getVariable('time')
    call nc_var%getAttribute('units', AttValues)
    ! AttValues looks like "<unit> since YYYY-MM-DD HH:MM:SS"
    call DIVIDE_STRING(trim(AttValues), ' ', strArr)
    !
    call DIVIDE_STRING(trim(strArr(3)), '-', date) 
    read(date(1),*) yRef
    read(date(2),*) mRef
    read(date(3),*) dRef

    ! allocate and initialize
    call nc_var%getData(timesteps)
    nTimeSteps = size(timesteps, dim=1)
    allocate(timepoints ( nTimeSteps ))
    timepoints = 0

    
    ! strArr(1) is <unit>
    ref_jday = date2dec(dd=dRef, mm=mRef, yy=yRef)
    do i = 1, nTimeSteps
       select case (strArr(1))
       case('hours')
          call dec2date(real(timesteps(i), dp)/real(DayHours, dp) + ref_jday, dd=d, mm=month, yy=year)
          timepoints(i) = timepoints(i) + nint( real(timesteps(i) - timesteps(1), dp) / real(DayHours, dp) , i4)
       case('days')
          call dec2date(real(timesteps(i), dp) + ref_jday, dd=d, mm=month, yy=year) 
          timepoints(i) = timepoints(i) + timesteps(i) - timesteps(1)
       case('months')
          d       = dRef ! set to dref as default
          month   = mod( (timesteps(i) + mRef ), YearMonths )
          year    = yRef + floor(real( timesteps(i) + mRef, dp) / real(YearMonths, dp) )
          ! correct month and year for december
          if (month .eq. 0 ) then
             month = 12
             year  = year - 1
          end if
          !
          if (i .EQ. 1) then
             timepoints(i) = 0
          else
             timepoints(i) = timepoints(i) +  nint(date2dec(dd=15 ,mm=month, yy=year) - date2dec(dd=15 ,mm=mStart, yy=yStart), i4)
          end if
       case DEFAULT
          call message('***ERROR: Time slicing in ' , trim(strArr(1)), ' not implemented!')
          stop
       end select

       ! save start year and month output timestamp
       if (i .EQ. 1) then
          yStart = year  
          mStart = month
          dStart = d
       end if
       if (i .EQ. nTimeSteps) then
         yEnd = year
         mEnd = month
         dEnd = d
       end if
    end do

    per_out = period_init(yStart, mStart, dStart, yEnd, mEnd, dEnd, timepoints, strArr(1))

    ! consistency check
    if (in_time_steps .ne. size(per_out%time_points, dim=1)) then
      call message('***ERROR: time steps of array and time axis are different by more than leap days')
      stop 1
    end if
    
  end subroutine get_time

  integer(i4) function n_cells_consistency(mask)

    use mo_message, only: message
    
    implicit none
    
    logical, intent(in) :: mask(:, :)
    
    n_cells_consistency = count( mask )
    if ( n_cells_consistency .eq. 0 ) then
      call message('***ERROR: no cell selected in mask')
      stop 1
    end if
  end function n_cells_consistency

  subroutine read_latlon(nc_in, lats, lons)

    use mo_kind, only: dp
    use mo_netcdf, only: NcDataset, NcVariable

    implicit none

    type(NcDataset), intent(in) :: nc_in ! NetCDF dataset
    real(dp), dimension(:,:), allocatable, intent(out) :: lats, lons ! latitude and longitude fields of input
    
    type(NcVariable) :: nc_var

    ! read lats and lon from file
    if (nc_in%hasVariable('lat')) then
      nc_var = nc_in%getVariable('lat')
      call nc_var%getData(lats)
    end if
    if (nc_in%hasVariable('lon')) then
      nc_var = nc_in%getVariable('lon')
      call nc_var%getData(lons)
    end if
    
  end subroutine read_latlon
  
end module mo_read
