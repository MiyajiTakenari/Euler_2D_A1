module globals
    real(8), save :: dt, res(3), time = 0.0d0
    real(8), allocatable, save :: x(:, :), y(:, :), bq(:, :, :), e(:, :, :), &
    & mx(:, :), my(:, :), nx(:, :), ny(:, :), s_j(:, :)
    integer, save :: exit_flag = 0
    ! x(-4:imax+2, -4:jmax+2), y(-4:imax+2, -4:jmax+2)
    ! bq(-2:imax+2, -2:jmax+2, 4), e(-1:imax, -1:jmax, 4)
end module globals