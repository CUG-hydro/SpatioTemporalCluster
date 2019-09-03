module mo_weight

PUBLIC :: wsum
PUBLIC :: wmean

INTERFACE wsum
    MODULE PROCEDURE wsum_vec, wsum_mat
END INTERFACE wsum

INTERFACE wmean
    MODULE PROCEDURE wmean_vec, wmean_mat
END INTERFACE wmean


contains

real function wsum_vec(mat, ws, mask)
    REAL   , dimension(:), intent(in) :: mat
    REAL   , dimension(:), optional, intent(in) :: ws
    LOGICAL, dimension(:), optional, intent(in) :: mask

    ! implicit none
    ! real :: args
    ! local
    REAL   , dimension(:), allocatable :: vec
    REAL   , dimension(:), allocatable :: w
    integer :: nrow, ncol, i, j
    real    :: tol
    LOGICAL :: is_weight 
    is_weight = present(ws)
    ! can't use print here

    if ( .not. present(mask) ) then
        ! no mask
        if (is_weight) then
            tol = sum(mat * (ws / sum(ws))) * size(ws)
        else
            tol = sum(mat)
        endif
    else
        vec = PACK(mat, mask)

        if (is_weight) then
            w   = PACK(ws, mask)
            ! allocate( w(count(mask)) )
            tol = sum(vec * (w / sum(w))) * size(w, 1)
            deallocate(w)
        else 
            tol = sum(vec)
        endif
        deallocate(vec)
    endif

    wsum_vec = tol
end function wsum_vec

real function wsum_mat(mat, ws, mask)
    ! implicit none
    ! real :: args
    REAL   , dimension(:, :), intent(in) :: mat
    REAL   , dimension(:, :), optional, intent(in) :: ws
    LOGICAL, dimension(:, :), optional, intent(in) :: mask

    ! local
    REAL   , dimension(:), allocatable :: vec
    REAL   , dimension(:), allocatable :: w
    
    integer :: nrow, ncol, i, j
    real    :: tol
    
    LOGICAL :: is_weight 
    is_weight = present(ws)

    if (.not. present(mask)) then
        if (is_weight) then
            tol = sum(mat * (ws / sum(ws))) * size(ws)
        else
            tol = sum(mat)
        endif
    else 
        vec = PACK(mat, mask)

        if (is_weight) then
            w   = PACK(ws, mask)
            ! allocate( w(count(mask)) )
            tol = sum(vec * (w / sum(w))) * size(w, 1)
            deallocate(w)
        else 
            tol = sum(vec)
        endif
        deallocate(vec)
    endif
    wsum_mat = tol
end function wsum_mat

real function wmean_vec(mat, ws, mask)
    ! implicit none
    REAL   , dimension(:), intent(in) :: mat
    REAL   , dimension(:), optional, intent(in) :: ws
    LOGICAL, dimension(:), optional, intent(in) :: mask

    real tol;
    integer n;
    ! local
    tol = wsum_vec(mat, ws, mask) 

    if (present(mask)) then
        n = count(mask)
    else
        n = size(mat)
    endif
    wmean_vec = tol / n
    ! write (120, *) tol, n, wmean, 11.0/2
end function wmean_vec

real function wmean_mat(mat, ws, mask)
    ! implicit none
    REAL   , dimension(:, :), intent(in) :: mat
    REAL   , dimension(:, :), optional, intent(in) :: ws
    LOGICAL, dimension(:, :), optional, intent(in) :: mask

    real tol;
    integer n;
    ! local
    tol = wsum_mat(mat, ws, mask) 

    if (present(mask)) then
        n = count(mask)
    else
        n = size(mat)
    endif
    wmean_mat = tol / n
end function wmean_mat

end module
