! !**********************************************************************
! !  PROGRAM: dCluster
! !
! !  PURPOSE:  cluster drought clusters in space and time
! !  DATE:     developed in Budapest, 10-11.03.2011
! !**********************************************************************
! subroutine ClusterEvolution2( SMIc, nrows, ncols, nMonths, nCells, cellCoor, nCellInter, thCellClus, idCluster2)
!   ! bind(C, name="clusterevolution_")

!   use mo_drought_evaluation
!   USE mo_kind,   only : i4, sp, dp

!   implicit none

!   ! input variables
!   ! integer(i4), dimension(:,:,:),            intent(inout) :: SMIc     ! Drought indicator
!   integer(i4), dimension(nrows,ncols,nMonths), intent(inout) :: SMIc    ! Drought indicator
!   integer(i4),                              intent(in)    :: nrows
!   integer(i4),                              intent(in)    :: ncols
!   integer(i4),                              intent(in)    :: nMonths
!   integer(i4),                              intent(in)    :: nCells     ! number of effective cells
!   ! integer(i4), dimension(:,:), allocatable, intent(in)    :: cellCoor
!   integer(i4), dimension(nCells,2),         intent(in)    :: cellCoor
!   integer(i4),                              intent(in)    :: nCellInter ! number cells for joining clusters in time
!   integer(i4),                              intent(in)    :: thCellClus ! treshold  for cluster formation in space
!   integer(i4), dimension(nrows,ncols,nMonths),intent(out) :: idCluster2 ! drought clusters id, returned for R

!   CALL ClusterEvolution(SMIc, nrows, ncols, nMonths, nCells, cellCoor, nCellInter, thCellClus)
! end subroutine ClusterEvolution2


!-------------------------------------------------------
! SVAT statistics
!-------------------------------------------------------
subroutine ClusterStats2( SMI, mask, SMI_thld, &
    nrows, ncols, nMonths, nCells, &
    mGridArea, &
    idCluster, nClusters, shortCnoList, &
    DAreaEvol2, DTMagEvol2)
    ! bind(C, name="clusterevolution_")

    use mo_drought_evaluation
    USE mo_kind,        only: i4, sp, dp
    use InputOutput,    only: DTMagEvol, DAreaEvol, aDA, aDD, TDM,     &
                              dASevol, nBasins, nEvents, eIdPerm, eventId, &
                              delStatistics
                              ! nClusters, idCluster, shortCnoList, &

    implicit none

    ! input variables
    ! integer(i4), dimension(:,:,:),            intent(inout) :: SMIc     ! Drought indicator
    ! real(sp)   , dimension(nrows*ncols, nMonths), intent(in) :: SMI
    logical    , dimension(nrows, ncols),         intent(in) :: mask
    real(sp)   , dimension(count(mask), nMonths), intent(in) :: SMI
    ! real(sp)   ,                                  intent(in) :: SMI_thld
    real(sp)   , dimension(nrows, ncols),         intent(in) :: SMI_thld

    integer(i4),                              intent(in)    :: nrows
    integer(i4),                              intent(in)    :: ncols
    integer(i4),                              intent(in)    :: nMonths
    integer(i4),                              intent(in)    :: nCells     ! number of effective cells
    ! integer(i4), dimension(:,:), allocatable, intent(in)    :: cellCoor
    ! integer(i4),                              intent(in)    :: nCellInter ! number cells for joining clusters in time
    ! integer(i4),                              intent(in)    :: thCellClus ! treshold  for cluster formation in space
    real(sp)   , dimension(nrows, ncols),     intent(in)    :: mGridArea

    integer(i4), dimension(nrows,ncols,nMonths),intent(in)  :: idCluster  ! drought clusters id, returned for R
    integer(i4),                                intent(in)  :: nClusters
    integer(i4), dimension(nClusters),          intent(in)  :: shortCnoList

    real(dp), dimension(nMonths,nClusters),     intent(out) :: DAreaEvol2    ! drought area evolution (fraction Germany)
    real(dp), dimension(nMonths,nClusters),     intent(out) :: DTMagEvol2    !         magnitud
    ! local variables
    ! integer(i4), dimension(nrows,ncols,nMonths)  :: SMIc    ! Drought indicator
    ! integer(i4), dimension(nCells,2)             :: cellCoor

    ! CALL droughtIndicator( SMI, mask, SMI_thld, nrows, ncols, nMonths, &
    !       cellCoor, SMIc)
    ! CALL ClusterEvolution(SMIc, nrows, ncols, nMonths, nCells, cellCoor, nCellInter, thCellClus)
    CALL ClusterStats( SMI, mask, nrows, ncols, nMonths, nCells, SMI_thld, mGridArea, &
        idCluster, nClusters, shortCnoList)

    ! idCluster2 = idCluster
    DAreaEvol2 = DAreaEvol
    DTMagEvol2 = DTMagEvol

    CALL delStatistics ! del statistic variables
end subroutine ClusterStats2
