module globals
    real(8), save :: dt, time = 0.0d0
    real(8), allocatable, save :: x(:, :), y(:, :), bq(:, :, :), e(:, :, :), f(:, :, :), &
    & mx(:, :), my(:, :), nx(:, :), ny(:, :), s_j(:, :), res_x(:, :), res_y(:, :)
    integer, save :: exit_flag = 0
    ! x(-4:imax+2, -4:jmax+2), y(-4:imax+2, -4:jmax+2)
    ! bq(-2:imax+2, -2:jmax+2, 4), e(-1:imax, -1:jmax, 4)
end module globals