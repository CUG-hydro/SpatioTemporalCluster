!> \file mo_smi_constants.f90

!> \brief Provides SMI specific constants

!> \details Provides SMI specific constants such as nodata values, treshholds for cluster calcul...

!> \author Matthias Zink
!> \date July 2014

MODULE mo_smi_constants

  USE mo_kind, ONLY: i4, sp, dp

  IMPLICIT NONE

  PRIVATE

  integer(i4), public, parameter :: nodata_i4          = -9999_i4  ! [-]     global no data value
  real(sp),    public, parameter :: nodata_sp          = -9999._sp ! [-]     global no data value
  real(dp),    public, parameter :: nodata_dp          = -9999._dp ! [-]     global no data value

  integer(i4), public, parameter :: YearMonths         = 12        ! months per year
  integer(i4), public, parameter :: DayHours           = 24        ! months per year


END MODULE mo_smi_constants
