!**********************************************************************
!  SOIL MOISTURE INDEX                                                *
!  PURPOSE     Estimate SMI based on monthly values of SM_mHM         *
!  CREATED     Luis Samaniego, 15.02.2011                             *
!  HISTORY     Stephan Thober, 16.12.2014 - added evalSMI capability  *
!              Stephan Thober, 07.11.2017 - added inversion of CDFs   *
!              Stephan Thober, 02.08.2019 - added non-full year       *
!**********************************************************************
module mo_smi
  !
  use mo_global_variables, only: period
  use mo_kind,          only: dp
  !
  implicit none
  ! public routines
  public :: optimize_width
  public :: calSMI
  public :: invSMI
  !

  private

  
contains

  ! subroutine for estimating SMI for first array
  subroutine optimize_width( opt_h, silverman_h, SM, nCalendarStepsYear, per_kde)
    use omp_lib
    use mo_kind,          only : i4, sp
    use mo_kernel,        only : kernel_density_h
    implicit none
    
    ! input variables
    logical,                  intent(in)    :: silverman_h ! optimize kernel width
    real(sp), dimension(:,:), intent(in)    :: SM
    integer(i4),              intent(in)    :: nCalendarStepsYear
    type(period),             intent(in)    :: per_kde

    ! output variables
    real(dp), dimension(:,:), intent(inout) :: opt_h

    ! local variables
    integer(i4)                             :: ii     ! cell index
    integer(i4)                             :: mm     ! month index
    real(dp), dimension(:),     allocatable :: X
    logical,                    allocatable :: t_mask_kde(:)
    integer(i4),                allocatable :: time_kde(:)


    call get_time_indizes(time_kde, per_kde, nCalendarStepsYear)

    !$OMP parallel default(shared) &
    !$OMP private(ii,mm,X, t_mask_kde)
    !$OMP do SCHEDULE(STATIC)
    do mm = 1, nCalendarStepsYear    

      t_mask_kde = (time_kde .eq. mm)
      
      do ii = 1, size( SM, 1 )
#ifdef SMIDEBUG      
         if ((mod(ii, 100) .eq. 0_i4) .and. (mm .eq. nCalendarStepsYear)) print *, ii, mm
#endif
         !
         allocate(X(count(t_mask_kde)))
         X(:) = pack(SM( ii, :), t_mask_kde)
         !
         ! determine kernel width if these have not been read from file
         opt_h(ii,mm) = kernel_density_h( X(:), silverman = silverman_h )
         deallocate(X)
       end do
    end do
    !$OMP end do
    !$OMP end parallel
  end subroutine optimize_width
 
  !!======================================================
  !! ESTIMATE SMI 
  !!======================================================
  !! subroutine for calculating SMI for second array with pdf of first one
  subroutine calSMI( hh, sm_kde, sm_eval, nCalendarStepsYear, SMI, per_kde, per_eval)
    
    use mo_kind,          only: i4, sp, dp
    use mo_kernel,        only: kernel_cumdensity
    use mo_smi_constants, only: nodata_dp, nodata_sp
    
    implicit none

    ! input variables
    real(dp), dimension(:,:), intent(in)  :: hh
    real(sp), dimension(:,:), intent(in)  :: sm_kde
    real(sp), dimension(:,:), intent(in)  :: sm_eval
    integer(i4),              intent(in)  :: nCalendarStepsYear
    type(period),             intent(in)  :: per_kde
    type(period),             intent(in)  :: per_eval

    ! output variables
    real(sp), dimension(:,:), intent(out) :: SMI

    ! local variables
    integer(i4)                           :: mm  ! loop index
    integer(i4)                           :: ii  ! cell index
    real(dp), dimension(:), allocatable   :: cdf
    real(dp), dimension(:), allocatable   :: X_kde
    real(dp), dimension(:), allocatable   :: X_eval
    integer(i4)                           :: n_time
    logical,                allocatable   :: t_mask_kde(:)
    integer(i4),            allocatable   :: time_kde(:)
    logical,                allocatable   :: t_mask_eval(:)
    integer(i4),            allocatable   :: time_eval(:)
    real(sp), dimension(:), allocatable   :: dummy_1d_sp

    call get_time_indizes(time_kde, per_kde, nCalendarStepsYear)
    call get_time_indizes(time_eval, per_eval, nCalendarStepsYear)
#ifdef SMIDEBUG
    print *, 'First 10 values of time_kde:'
    print *, time_kde(:10)
    print *, 'First 10 values of time_eval:'
    print *, time_eval(:10)
#endif
    
    do mm = 1,  nCalendarStepsYear   ! time loop

      t_mask_kde = (time_kde .eq. mm)
      t_mask_eval = (time_eval .eq. mm)
      n_time = count(t_mask_eval)
#ifdef SMIDEBUG
      print *, 'Processing: timestep, number of eval timesteps, number of kde timesteps'
      print *, mm, n_time, count(t_mask_kde)
#endif       
      
      ! check whether there is data for that timestep to be calculated
      if (n_time .eq. 0_i4) cycle

      call cellSMI(SM_kde, t_mask_kde, SM_eval, t_mask_eval, hh(:, mm), SMI)
    end do

    ! do leap days
    if (per_eval%n_leap_days .gt. 0) then

      mm = 60 ! take cdf of March first
      t_mask_kde = (time_kde .eq. mm) 
      t_mask_eval = (time_eval .eq. -1_i4)
      n_time = count(t_mask_eval)

      call cellSMI(SM_kde, t_mask_kde, SM_eval, t_mask_eval, hh(:, mm), SMI)
    end if

  end subroutine calSMI

  subroutine cellSMI(SM_kde, t_mask_kde, SM_eval, t_mask_eval, hh, SMI)

    use mo_kind, only: i4, sp
    use mo_kernel, only: kernel_cumdensity
    use mo_smi_constants, only: nodata_sp
    
    implicit none

    real(sp), intent(in) :: SM_kde(:, :), SM_eval(:, :)
    logical,  intent(in) :: t_mask_kde(:), t_mask_eval(:)
    real(dp), intent(in) :: hh(:)
    real(sp), intent(inout) :: SMI(:, :)

    integer(i4)                           :: ii  ! cell index
    real(dp), dimension(:), allocatable   :: cdf
    real(dp), dimension(:), allocatable   :: X_kde
    real(dp), dimension(:), allocatable   :: X_eval
    real(sp), dimension(:), allocatable   :: dummy_1d_sp
    

    !$OMP parallel default(shared) &
    !$OMP private(ii, X_kde, X_eval, cdf, dummy_1d_sp)
    allocate ( X_kde       (count(t_mask_kde)))
    allocate ( X_eval      (count(t_mask_eval)))
    allocate ( cdf         (count(t_mask_eval)))
    allocate ( dummy_1d_sp (size(t_mask_eval, dim=1)))

    !$OMP do SCHEDULE(STATIC)
    do ii = 1, size(SM_kde,1)          ! cell loop

#ifdef SMIDEBUG      
      if ((mod(ii, 10000) .eq. 0_i4)) print *, ii
#endif
      
      X_kde(:)  = pack(real( SM_kde(ii,:), dp), t_mask_kde)
      X_eval(:) = pack(real(SM_eval(ii,:), dp), t_mask_eval)
        
      cdf(:) = kernel_cumdensity(x_kde, hh(ii), xout=x_eval)
        
      dummy_1d_sp = unpack(real(cdf(:), sp), t_mask_eval, nodata_sp)
      SMI(ii, :) = merge(dummy_1d_sp, SMI(ii, :), t_mask_eval)
      
    end do
    !$OMP end do
    !$OMP end parallel

    ! arrays are deallocated if openmp is active
    if (allocated(x_kde))       deallocate( x_kde)
    if (allocated(x_eval))      deallocate( X_eval)
    if (allocated(cdf))         deallocate( cdf)
    if (allocated(dummy_1d_sp)) deallocate( dummy_1d_sp )

  end subroutine cellSMI

  ! create objective function of kernel_cumdensity and minimize it using
  ! nelmin because function is monotone
  subroutine invSMI(sm_kde, hh, SMI_invert, nCalendarStepsYear, per_kde, per_smi, SM_invert)

    use mo_kind,          only: i4, sp, dp
    use mo_message,       only: message
    use mo_smi_constants, only: nodata_sp, nodata_dp
    use mo_kernel,        only: kernel_cumdensity
    
    implicit none
    
    ! input variables
    real(dp), dimension(:,:),              intent(in)  :: hh
    real(sp), dimension(:,:),              intent(in)  :: sm_kde
    real(sp), dimension(:,:),              intent(in)  :: SMI_invert
    integer(i4),                           intent(in)  :: nCalendarStepsYear
    type(period),                          intent(in)  :: per_kde
    type(period),                          intent(in)  :: per_smi

    ! output variables
    real(sp), dimension(:,:), allocatable, intent(out) :: SM_invert

    ! local variables
    logical,                allocatable :: t_mask_kde(:), t_mask_smi(:)
    integer(i4),            allocatable :: time_kde(:), time_smi(:)
    integer(i4)                         :: n_cells
    integer(i4)                         :: n_years_kde, n_years_invert
    integer(i4)                         :: ii, yy, mm ! loop variables
    integer(i4)                         :: xx_n_sample
    integer(i4), dimension(1)           :: idx_invert
    real(sp), dimension(:), allocatable :: dummy_1d_sp
    real(dp)                            :: hh_kde
    real(dp)                            :: xx_min, xx_max, xx_h
    real(dp), dimension(:), allocatable :: y_inv, x_inv
    real(dp), dimension(:), allocatable :: xx_cdf, yy_cdf
    real(dp), dimension(:), allocatable :: xx_kde

    ! initialize extents
    n_cells        = size(SMI_invert, 1)
    xx_n_sample    = 2000_i4 ! gives precision of at least 0.0005 in SM
    allocate(xx_cdf(xx_n_sample))
    allocate(yy_cdf(xx_n_sample))
    xx_cdf = nodata_dp
    yy_cdf = nodata_dp

    ! initialize output array
    allocate(SM_invert(n_cells, size(SMI_invert, 2)))
    SM_invert = nodata_sp

    ! get time indizes
    call get_time_indizes(time_kde, per_kde, nCalendarStepsYear)
    call get_time_indizes(time_smi, per_smi, nCalendarStepsYear)

    call message('')
    call message('  start inversion of CDF')
    !$OMP parallel default(shared) &
    !$OMP private(mm, yy, xx_kde, hh_kde, y_inv, x_inv, dummy_1d_sp, xx_min, xx_max, xx_h, xx_cdf, yy_cdf, idx_invert)
    !$OMP do
    do mm = 1, nCalendarStepsYear

      t_mask_kde = (time_kde .eq. mm)
      t_mask_smi = (time_smi .eq. mm)
      if (count(t_mask_smi) .eq. 0_i4) cycle

      allocate(xx_kde(count(t_mask_kde)))
      allocate(y_inv(count(t_mask_smi)))
      allocate(x_inv(count(t_mask_smi)))
      allocate(dummy_1d_sp(count(t_mask_smi)))
      
      do ii = 1, n_cells
        if (modulo(ii, 1000) .eq. 0) print *, ii, n_cells
        xx_kde(:) = pack(real(SM_kde( ii, : ), dp), t_mask_kde)
        hh_kde    = hh(ii, mm)
        y_inv(:)  = pack(real(SMI_invert(ii, :), dp), t_mask_smi)
        
        ! sample cdf
        xx_min    = max(0._dp, minval(xx_kde - 10._dp * hh_kde))
        xx_max    = min(1._dp, maxval(xx_kde + 10._dp * hh_kde))
        xx_h      = (xx_max - xx_min) / real(xx_n_sample, dp)
        do yy = 1, xx_n_sample
           xx_cdf(yy) = xx_min + (yy - 1_i4) * xx_h
        end do
        xx_cdf = merge(1._dp, xx_cdf, xx_cdf .gt. 1._dp)
        yy_cdf = kernel_cumdensity(xx_kde, hh_kde, xout=xx_cdf)
        
        do yy = 1, size(y_inv, 1)
          idx_invert = minloc(abs(y_inv(yy) - yy_cdf))
          x_inv(yy) = xx_cdf(idx_invert(1))
        end do

        dummy_1d_sp = unpack(real(x_inv(:), sp), t_mask_smi, nodata_sp)
        SM_invert(ii, :) = merge(dummy_1d_sp, SM_invert(ii, :), t_mask_smi)

      end do

      deallocate(xx_kde, y_inv, x_inv, dummy_1d_sp)
      
    end do
    !$OMP end do
    !$OMP end parallel
    call message('  finish inversion of CDF... ok')
    call message('')

    ! free memory
    deallocate(xx_cdf, yy_cdf)
    
  end subroutine invSMI

  
  subroutine get_time_indizes(time, per, nCalendarStepsYear)

    use mo_kind,             only: i4, dp
    use mo_global_variables, only: period
    use mo_julian,           only: dec2date, date2dec
    
    implicit none

    integer(i4),              intent(in)  :: nCalendarStepsYear
    type(period),             intent(in)  :: per
    integer(i4), allocatable, intent(out) :: time(:)
    integer(i4) :: ii, jj, start, dd, mm

    ! remove leap days
    allocate(time(size(per%time_points, dim=1)))
    time(:) = 0_i4
    
    if (nCalendarStepsYear .eq. 12_i4) then
      start = per%m_start
    else
      start = int(per%j_start - date2dec(31, 12, per%y_start - 1), i4)
      ! account for removed leap days
      if ((int((date2dec(31, 12, per%y_start) - date2dec(1, 1, per%y_start) + 1), i4) .eq. 366) .and. per%m_start .gt. 2) then
        start = start - 1
      end if
    end if

    jj = start
    do ii = 1, size(time)
      time(ii) = jj
      call dec2date(real(per%j_start + per%time_points(ii) - 1, dp), dd=dd, mm=mm)
      if ((dd .eq. 29) .and. (mm .eq. 2)) then
        time(ii) = -1
        cycle
      end if
      jj = jj + 1_i4
      if (jj .gt. nCalendarStepsYear) jj = 1_i4
    end do
    
  end subroutine get_time_indizes
  
end module mo_smi
