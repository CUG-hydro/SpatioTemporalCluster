MODULE mo_drought_evaluation

  USE mo_kind,   only : i4, sp, dp

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: droughtIndicator
  PUBLIC :: ClusterEvolution
  PUBLIC :: ClusterStats
  PUBLIC :: calSAD

  ! ------------------------------------------------------------------

CONTAINS

  ! ------------------------------------------------------------------


!*********************************************************************
!  PURPOSE: Find drought clusters and statistics
!           1) truncate SMIp < SMI_th
!
!  Luis Samaniego, created  28.02.2011
!                  updated  05.10.2011
!*********************************************************************
subroutine droughtIndicator( SMI, mask, SMI_thld, cellCoor , SMIc)

  use mo_smi_constants, only : nodata_sp, nodata_i4
  use mo_utils,         only : lesserequal, notequal

  implicit none

  ! input variable
  real(sp),    dimension(:,:),                intent(in)  :: SMI
  logical,     dimension(:,:),                intent(in)  :: mask
  real(sp),                                   intent(in)  :: SMI_thld
  integer(i4), dimension(:,:), allocatable,   intent(out) :: cellCoor
  integer(i4), dimension(:,:,:), allocatable, intent(out) :: SMIc       ! Drought indicator
  
  ! local variables
  real(sp), dimension(size(mask, dim=1),&
                      size(mask, dim=2))        :: dummy_2d_sp
  integer(i4)                                   :: i, j, m, k
  integer(i4)                                   :: nrows
  integer(i4)                                   :: ncols
  integer(i4)                                   :: nMonths

  ! initialize
  nrows   = size( mask, 1 )
  ncols   = size( mask, 2 )
  nMonths = size( SMI,  2 )
  !
  print *, 'Threshold for drought identification: ', SMI_thld
  allocate ( SMIc( nrows, ncols, nMonths) )
  
  do m = 1, nMonths
     dummy_2d_sp = unpack(SMI(:,m), mask, nodata_sp)
     ! filter for possible error within domain
     ! where (SMI(:,:,m) .le. SMI_thld .and. SMI(:,:,m) .ne. nodata_sp )
     where ( (lesserequal(dummy_2d_sp, SMI_thld)) .and. (notequal(dummy_2d_sp, nodata_sp)) )
        SMIc(:,:,m) = 1
     elsewhere ( notequal(dummy_2d_sp, nodata_sp) )
        SMIc(:,:,m) = 0
     elsewhere
        SMIc(:,:,m) = nodata_i4
     end where
  end do

  !
  allocate (cellCoor( count(mask),2 ))
  k = 0
  do j=1,ncols
     do i=1,nrows
        if ( mask(i,j)) then
           k = k + 1
           cellCoor(k,1) = i
           cellCoor(k,2) = j
           ! if (k==7645.or.k==2479) print*, i,j,k
        end if
     end do
  end do
  !
end subroutine droughtIndicator

!**********************************************************************
!  PROGRAM: dCluster
!
!  PURPOSE:  cluster drought clusters in space and time
!  DATE:     developed in Budapest, 10-11.03.2011
!**********************************************************************
subroutine ClusterEvolution( SMIc, nrows, ncols, nMonths, nCells, cellCoor, nCellInter, thCellClus)

  ! use numerical_libraries, only                        : SVIGN
  use mo_sort,          only : sort
  use mo_smi_constants, only : nodata_i4
  use InputOutput, only                                : idCluster, &
                                                         shortCnoList, &
                                                         nClusters
                                                         
 
 implicit none

  ! input variables
  integer(i4), dimension(:,:,:),            intent(inout) :: SMIc     ! Drought indicator
  integer(i4),                              intent(in)    :: nrows
  integer(i4),                              intent(in)    :: ncols
  integer(i4),                              intent(in)    :: nMonths
  integer(i4),                              intent(in)    :: nCells   ! number of effective cells
  integer(i4), dimension(:,:), allocatable, intent(in)    :: cellCoor
  integer(i4),                              intent(in)    :: thCellClus ! treshold  for cluster formation in space
  integer(i4),                              intent(in)    :: nCellInter ! number cells for joining clusters in time

  
  ! local variables
  integer(i4)                              :: i, j, t
  integer(i4), dimension(:),   allocatable :: nC
  integer(i4), parameter                   :: factor = 1000
  integer(i4)                              :: ncInter
  integer(i4), dimension(:,:), allocatable :: cno
  integer(i4), dimension(:),   allocatable :: cnoList
  integer(i4), dimension(:),   allocatable :: vec
  integer(i4)                              :: maxNc, nTotal, idRep
  integer(i4), dimension(:,:), allocatable :: idCnew


  if (.not. allocated(idCluster) )  allocate   (idCluster(nrows, ncols, nMonths))
  if (.not. allocated(nC) )         allocate   (nC(nMonths))
  if ( allocated(cno) )             deallocate (cno, vec, cnoList)
  if (.not. allocated(idCnew))      allocate   (idCnew(nrows, ncols))

  ! ---------------------------------------------
  ! CLUSTERING IN SPACE and remove small clusters
  ! ---------------------------------------------
  do t=1,nMonths
     if ( count(SMIc(:,:,t) == 1) < thCellClus) then
        nC(t) = 0
        idCluster(:,:,t) = nodata_i4
        cycle
     end if
     call findClusters (cellCoor, thCellClus, t, idCluster(:,:,t), nC(t), nrows, ncols, nCells, SMIc)
     !print*, 'Finding clusters time :', t, nC(t)
  end do
  print*, 'Clusters in space done ...'
  !
  ! maximum number of clusters at all time steps
  maxNc = maxval(nC(:))
  nTotal = nMonths*maxNc
  allocate ( cno(nMonths,maxNc), vec(nTotal), cnoList(nTotal) )
  cno = -9
  !
  ! set unique cluster numbers and store them
  ! NOTE 
  !      e.g. month*factor + running nr.,
  !           5*1000+1 = 5001              => fisrt cluster in 5th month
  ! 
  do t=1,nMonths
     if (nC(t) == 0) cycle
     do i=1, nC(t)
        cno(t,i) = t*factor + i
        where ( idCluster(:,:,t) == i ) idCluster(:,:,t) = cno(t,i)
     end do
  end do
  ! ---------------------------------
  ! FIND CLUSTERS IN TIME
  ! determine intersection sets, join
  ! ---------------------------------
  do t=2,nMonths
     do i=1,nC(t)
        if (cno(t,i) == -9) cycle
        do j=1,nC(t-1)
           if (cno(t-1,j) == -9) cycle
           ncInter = count ( idCluster(:,:,t) == cno(t,i) .and. idCluster(:,:,t-1) == cno(t-1,j) )
           if ( ncInter >= nCellInter ) then
              ! renumber all from 1 to t
              where ( idCluster(:,:,1:t) == cno(t-1,j) ) idCluster(:,:,1:t) =  cno(t,i)
              ! rename cluster id from cno
              where ( cno(1:t,:) == cno(t-1,j) ) cno(1:t,:) = cno(t,i)
           end if
        end do
     end do
  end do
  print*, 'Clustering in time done ...'
  ! --------------------------
  ! COMPILE CLUSTER IDS + list
  ! --------------------------
  vec = -9
  cnoList = pack (cno, mask = cno > 0, vector=vec)
  !
  ! 1. sort
  idRep = -9
  forall(i=1:nTotal) vec(i) = i
  ! call SVIGN(nTotal,cnoList,cnoList)
  call sort(cnoList)
  ! 2.  remove repeated values
  do i=nTotal, 2,-1
     if (cnoList(i) < 0 ) cycle
     if (.not. cnoList(i-1) == cnoList(i) ) then
        if (idRep > 0) idRep = -9
        cycle
     end if
     if (idRep == -9) idRep = cnoList(i)
     cnoList(i) = -9
  end do
  ! call SVIGN(nTotal,cnoList,cnoList)
  call sort(cnoList)
  !
  ! 3. final consolidated list
  nClusters = count(cnoList > 0)
  allocate ( shortCnoList(nClusters) )
  shortCnoList = pack(cnoList, mask = cnoList > 0)
  !
  print*, '# Consolidated Clusters :', nClusters
  !
  deallocate(vec, nC, cnoList, cno )
end subroutine ClusterEvolution

!-------------------------------------------------------
! SVAT statistics
!-------------------------------------------------------
subroutine ClusterStats( SMI, mask, nrows, ncols, nMonths, nCells, SMI_thld )

  !  use numerical_libraries, only                       : SVIGN , DSVRGN, DEQTIL
  use mo_sort,          only : sort_index

  use mo_smi_constants, only: nodata_sp
  use InputOutput,      only: aDA, aDD, TDM, DTMagEvol, DAreaEvol,     &
                              nClusters, idCluster, shortCnoList, &
                              dASevol, nBasins, nEvents, eIdPerm, eventId

  implicit none

  ! input variables

  real(sp), dimension(:,:),    intent(in) :: SMI
  logical,  dimension(:,:),    intent(in) :: mask
  integer(i4),                 intent(in) :: nrows
  integer(i4),                 intent(in) :: ncols
  integer(i4),                 intent(in) :: nMonths
  integer(i4),                 intent(in) :: nCells      ! number of effective cell
!  integer(i4), dimension(:,:), intent(in) :: Basin_Id    ! IDs for basinwise drought analysiss
  real(sp),                    intent(in) :: SMI_thld    ! SMI threshold for clustering

  ! local variables
  real(sp), dimension(nrows, ncols)                         :: dummy_2d_sp
  integer(i4)                                               :: ic, i
  integer(i4)                                               :: t
  integer(i4) , dimension(:), allocatable                   :: counterA
  integer(i4) , dimension(:,:), allocatable                 :: aDDG
  real(dp) , dimension(:,:), allocatable                    :: mSev

  integer(i4)                                               :: eCounter

  integer(i4), dimension(:), allocatable                    :: vec

  allocate ( aDDG     (nrows, ncols) )
  allocate ( DAreaEvol(nMonths,nClusters) )
  allocate ( DTMagEvol(nMonths,nClusters) )
  allocate ( aDD      (nClusters)   )
  allocate ( aDA      (nClusters)   )
  allocate ( counterA (nClusters)   )
  allocate ( TDM      (nClusters)   )
  allocate ( dASevol  (nMonths,2,nBasins+1)   )       ! (area, severity) whole Germany => nBasins+1
  allocate ( mSev     (nrows, ncols) )      ! mean  severity
  !
  DAreaEvol = 0.0_dp
  DTMagEvol = 0.0_dp
  counterA  = 0
  do ic = 1, nClusters
     !print*, 'Statistics of cluster : 'shortCnoList(ic)
     aDDG = 0
     do t = 1, nMonths
        where (idCluster(:,:,t) == shortCnoList(ic) )
           ! duration
           aDDG = aDDG + 1
        end where
        !
        !  drought area evolution
        DAreaEvol(t,ic) =  real( count(idCluster(:,:,t) == shortCnoList(ic) ), dp) / real(nCells, dp)
        if (DAreaEvol(t,ic) > 0.0_dp) counterA(ic) = counterA(ic) + 1

        ! total magnitude (NEW  SM_tr -SMI) !!!
        dummy_2d_sp = unpack(SMI(:,t), mask, nodata_sp)
        DTMagEvol(t,ic) = sum( (SMI_thld - dummy_2d_sp), mask = idCluster(:,:,t) == shortCnoList(ic) )
     end do

     ! AVERAGE (MEAN) DURATION
     aDD(ic) = real( sum(aDDG, mask = aDDG > 0),dp ) / real( count(aDDG > 0), dp)

     ! TOTAL MAGNITUD (sum over space and time of SMI over all
     !                 cells afected by the event ic )
     TDM(ic) = sum( DTMagEvol(:,ic) )
  end do
  !  AVERAGE (MEAN) DROUGHT AREA per cluster event
  aDA = sum(DAreaEvol, DIM=1) / real(counterA, dp)
  !
  ! (NEW) Time evolution of the area and monthly severity
  ! whole domain
  do t = 1, nMonths
     dASevol(t,1,nBasins+1) = real( count( idCluster(:,:,t) > 0 ), dp ) / real( nCells, dp) * 1e2_dp
     dummy_2d_sp = unpack(SMI(:,t), mask, nodata_sp)
     where (idCluster(:,:,t) > 0 )
        mSev(:,:) = 1.0_dp - real(dummy_2d_sp, dp)
     end where
     dASevol(t,2,nBasins+1) = sum( mSev(:,:), mask = idCluster(:,:,t) > 0 ) / real( nCells, dp)  
  end do
  ! ! evolution at basin wise
  ! do t = 1, nMonths
  !   do i = 1, nBasins
  !     dASevol(t,1,i) = real ( count( idCluster(:,:,t) > 0 .and.  Basin_Id(:,:) == i  ), dp) &
  !                      / real(count(Basin_Id(:,:) == i), dp)  * 1e2_dp
  !     where (idCluster(:,:,t) > 0 .and.  Basin_Id(:,:) == i )
  !        mSev(:,:) = 1.0_dp - SMI(:,:,t)
  !     end where
  !     dASevol(t,2,i) = sum( mSev(:,:), mask = idCluster(:,:,t) > 0 .and. Basin_Id(:,:) == i ) &
  !                     / real(count(Basin_Id(:,:) == i), dp)
  !   end do
  ! end do


     !!! ORIGINALLY in SAD anaylysis
     nEvents = count (DAreaEvol .gt. 0.0_dp )
     allocate (  eIdPerm   (nEvents)    )
     allocate (  eventId   (nEvents, 3) )                !  dim1: running Nr.
                                                         !  dim2: 1 == cluster Id,
                                                         !        2 == month of ocurrence,
                                                         !        3 == number of cells
  
     ! keep event identification
     eCounter = 0
     do ic = 1,  nClusters
        do t = 1, nMonths
           if (DAreaEvol(t,ic) > 0.0_dp ) then
              eCounter = eCounter + 1
              eventId(eCounter,1) = shortCnoList(ic)
              eventId(eCounter,2) = t
              eventId(eCounter,3) = count( idCluster(:,:,t) == shortCnoList(ic) )
           end if
        end do
     end do
     ! sort event in order of area magnitude
     forall(i=1:nEvents) eIdPerm(i) = i
     allocate (vec(nEvents))
     ! call SVIGP (nEvents, eventId(:,3), vec, eIdPerm)
     eIdPerm = sort_index( eventID(:,3) )
     deallocate (vec)

     print*, 'Cluster statistics were estimated ... '
     deallocate ( counterA, aDDG, mSev )
end subroutine ClusterStats

!-------------------------------------------------------
! SAD analysis
!-------------------------------------------------------
subroutine calSAD(SMI, mask, iDur, nrows, ncols, nMonths, nCells, deltaArea, cellsize)

  ! use numerical_libraries, only                        : DSVRGN, DEQTIL, SVIGP
  use mo_sort,          only : sort      !, sort_index
  use mo_percentile,    only : percentile
  use mo_smi_constants, only : nodata_dp 

  use InputOutput, only                                : shortCnoList, &
                                                         nInterArea, nEvents, nClusters,  idCluster, &
                                                         SAD, SADperc, DAreaEvol, severity, nDsteps, &
                                                         durList, eventId, eIdPerm, &
                                                         nLargerEvents, nQProp, QProp

  implicit none

  ! input variable
  real(sp),    dimension(:,:), intent(in) :: SMI
  logical,     dimension(:,:), intent(in) :: mask
  integer(i4),                 intent(in) :: iDur
  integer(i4),                 intent(in) :: nrows
  integer(i4),                 intent(in) :: ncols
  integer(i4),                 intent(in) :: nMonths
  integer(i4),                 intent(in) :: nCells  ! number of effective cells
  integer(i4),                 intent(in) :: deltaArea  ! number of cells per area interval
  real(sp),                    intent(in) :: cellsize
  
  ! local variable
  real(dp),    dimension(nrows,ncols, nMonths)        :: SMI_unpack
  integer(i4)                                         :: t, i, k
!  integer(i4)                                         :: ic
  integer(i4)                                         :: iDc
  integer(i4)                                         :: d
  integer(i4)                                         :: ms, me
  integer(i4)                                         :: ke
  integer(i4)                                         :: eC
  integer(i4)                                         :: eCounter

  integer(i4)                                         :: ncic
  integer(i4)                                         :: nIntA
  real(dp), dimension(:), allocatable                 :: sevP
  !
  integer(i4)                                         :: nObs
!  integer(i4), dimension(:), allocatable              :: vec
  !
  do i = 1, size(SMI,2)
     SMI_unpack(:,:,i) = unpack(real(SMI(:,i), dp), mask, nodata_dp)
  end do
  
  if (iDur == 1) then
     ! set up SAD curves
     nInterArea = int( ceiling( real(nCells,dp) / real(deltaArea, dp) ), i4 )
     !
     ! total number of events to be evaluated
 !    nEvents = count (DAreaEvol > 0.0_dp )
     nLargerEvents = nEvents
 !     allocate (  eventId   (nEvents, 3) )                !  dim1: running Nr.
 !                                                         !  dim2: 1 == cluster Id,
 !                                                         !        2 == month of ocurrence,
 !                                                         !        3 == number of cells
 ! 
 !    allocate (  eIdPerm   (nEvents)    )
     allocate (  SAD       (nInterArea, 2, nLargerEvents ) )    !  dim2: 1 == area, 2 == Severity
     allocate (  SADperc   (nInterArea, nQProp ) )
     !
     ! ! keep event identification
     ! eCounter = 0
     ! do ic = 1,  nClusters
     !    do t = 1, nMonths
     !       if (DAreaEvol(t,ic) > 0.0_dp ) then
     !          eCounter = eCounter + 1
     !          eventId(eCounter,1) = shortCnoList(ic)
     !          eventId(eCounter,2) = t
     !          eventId(eCounter,3) = count( idCluster(:,:,t) == shortCnoList(ic) )
     !       end if
     !    end do
     ! end do
     ! ! sort event in order of magnitude
     ! forall(i=1:nEvents) eIdPerm(i) = i
     ! allocate (vec(nEvents))
     ! ! call SVIGP (nEvents, eventId(:,3), vec, eIdPerm)
     ! eIdPerm = sort_index( eventID(:,3) )
     ! deallocate (vec)
  end if
  !
  ! SAD
  print*, 'SAD for duration started: ', durList (iDur)
  SAD     = -9.0_dp
  SADperc = -9.0_dp
  nDsteps = ceiling (real(nMonths,dp) / real(durList (iDur),dp) )
  if ( allocated (severity) )  deallocate(severity)
  allocate ( severity (nrows, ncols, nDsteps ) )
  severity = nodata_dp
  !
  ! estimate severities for a given duration over all time steps
  do d = 1, nDsteps
     ms = (d-1)*durList(iDur) + 1
     me = ms + durList(iDur) - 1
     if (me > nMonths) me = nMonths
     !where ( ne(SMI_unpack(:,:,1), nodata_dp) )
     where ( mask )
        severity(:,:,d) = 1.0_dp - sum( SMI_unpack(:,:,ms:me), DIM=3 ) / real( me-ms+1, dp )
     end where
  end do
  !
  ! estimate curves for larger events ONLY
  do eC = 1, nLargerEvents
     eCounter = eIdPerm( nEvents + 1 - eC )
     iDc = eventId(eCounter,1)
     t   = eventId(eCounter,2)
     d   = (t-1)/durList (iDur) + 1
     ! number of cells of the event in eCounter
     ncic = eventId(eCounter,3)
     if (allocated (sevP)) deallocate(sevP)
     allocate (sevP(ncic))
     sevP = pack (severity(:,:,d), mask = idCluster(:,:,t) == iDc )
     !
     ! ----------------------------------------------
     ! fast approach
     ! rank severities and estimate cummulative areas
     ! ----------------------------------------------
     ! call DSVRGN (ncic,sevP,sevP)
     call sort( sevP )
     nIntA = int( ceiling( real(ncic,dp) / real(deltaArea, dp) ), i4 )
     do k = 1, nIntA
        ke = k*deltaArea
        if (ke > ncic ) ke = ncic
        SAD(k, 1, eC ) = real(ke, dp) *  real(cellsize,dp)**2.0_dp  ! cumulative area
        SAD(k, 2, eC ) = sum( sevP(ncic+1-ke:ncic) ) / real(ke, dp) ! sorted by incresing value
     end do
  end do
  !
  ! estimate percentilles for all intervals
  do k = 1, nInterArea
     nObs = count(SAD(k,2,:) > 0.0_dp)
     if (.not. nObs > 1) cycle
     if (allocated (sevP)) deallocate(sevP)
     allocate (sevP(nObs))
     sevP = pack (SAD(k,2,:), mask = SAD(k,2,:) > 0.0_dp )
     ! call DEQTIL (nObs, sevP, nQprop, Qprop, SADperc(k,:), Xlo, Xhi, nMiss)
     SADperc(k,:) =  percentile( sevP, Qprop )
  end do
  !
  if ( allocated(sevP) ) deallocate ( sevP )
end subroutine calSAD

!-------------------------------------------------------
! clustering in space
! assign a unique running number at time t
!-------------------------------------------------------
subroutine findClusters (cellCoor, thCellClus, t,iC,nCluster, nrows, ncols, nCells, SMIc)

  !use InputOutput,      only : cellCoor, thCellClus, SMIc
  use mo_smi_constants, only : nodata_i4

  implicit none

  integer(i4), dimension(:,:),                intent(in)    :: cellCoor
  integer(i4),                                intent(in)    :: thCellClus ! treshold  for cluster formation
  integer(i4),                                intent(in)    :: nrows
  integer(i4),                                intent(in)    :: ncols
  integer(i4),                                intent(in)    :: nCells     ! number of effective cells
  integer(i4),                                intent(in)    :: t          !  time step
  integer(i4), dimension(:,:,:),              intent(inout) :: SMIc       ! SMI indicator
  integer(i4),                                intent(out)   :: nCluster   !  number of clusters larger than a threshold
  integer(i4), dimension(nrows,ncols),        intent(out)   :: iC

  ! local variables
  integer(i4)                                           :: i, j, k, klu, klu1
  integer(i4)                                           :: iul, idr, jul, jdr
  integer(i4)                                           :: krow, kcol
  integer(i4), dimension(:), allocatable                :: cno, vec
  integer(i4), dimension(:), allocatable                :: nCxCluster
  integer(i4)                                           :: nClusterR, ncc
  !
  iC = nodata_i4
  nCluster = 0
  do k = 1, nCells
     krow = cellCoor(k,1)
     kcol = cellCoor(k,2)
     iul  = max(krow-1,1)
     idr  = min(krow+1,nrows)
     jul  = kcol
     jdr  = min(kcol+1,ncols)
     ! SMIc of k
     klu  = SMIc(krow, kcol, t)
     if (klu /= 1) cycle
     if ( (klu == 1) .and. (iC(krow, kcol) == nodata_i4) ) then
        nCluster=nCluster+1
        iC(krow, kcol) = nCluster
     end if
     do j=jul, jdr
        do i= iul, idr
           if (iC(i,j) == nodata_i4 .and. &
                SMIc(i,j,t) == klu                     ) then
              iC(i,j) = iC(krow, kcol)
           end if
        end do
     end do
  end do

  ! consolidate clusters
  if ( nCluster > 0 ) then
     allocate ( cno(nCluster), vec(nCluster) )
     cno = (/(i, i=1,nCluster)/)
     vec = -9
     nClusterR = nCluster

     do k = nCells, 1, -1
        krow = cellCoor(k,1)
        kcol = cellCoor(k,2)
        klu  = iC(krow, kcol)
        iul  = max(krow-1,1)
        idr  = min(krow+1,nrows)
        jul  = kcol
        jdr  = min(kcol+1,ncols)
        do j=jul, jdr
           do i= iul, idr
              klu1 = iC(i, j)
              if ( klu  /= nodata_i4 .and. &
                   klu1 /= nodata_i4 .and. &
                   klu  /= klu1                       ) then
                 cno(klu1) = -9
                 nClusterR = nClusterR - 1
                 where ( iC == klu1 ) iC = klu
              end if
           end do
        end do
     end do
     !
     ! delete small clusters < thesh. area
     do i=1, nCluster
        if (cno(i) == -9 ) cycle
        ncc = count(iC == cno(i))
        if ( ncc <= thCellClus ) then
           where (iC == cno(i)) iC = nodata_i4
           cno(i) = -9
           nClusterR = nClusterR -1
        end if
     end do
     !
     ! reordering
     cno = pack (cno, mask = cno > 0, vector=vec)
     where (cno <= 0) cno = 0
     !
     !
     allocate (nCxCluster(nClusterR))
     do i=1, nClusterR
        where (iC == cno(i)) iC = i
        nCxCluster(i) = count(iC == i)
     end do
     !
     deallocate (cno, nCxCluster)
  end if
  nCluster = nClusterR
end subroutine findClusters

END MODULE mo_drought_evaluation
