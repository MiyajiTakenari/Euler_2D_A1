module globals
    real(8), save :: dt, time = 0.0d0
    !real(8), save :: rec(-1:100, 50)
    !(x, y, 成分)
    real(8), allocatable, save :: x(:, :), y(:, :), bq(:, :, :), e(:, :, :), f(:, :, :), &
    & mx(:, :), my(:, :), nx(:, :), ny(:, :), s_j(:, :), &
    & bq_n(:, :, :), res_x(:, :), res_y(:, :)
    integer, save :: exit_flag = 0
end module globals