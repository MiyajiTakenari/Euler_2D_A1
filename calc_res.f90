subroutine calc_res
    use params
    use globals

    implicit none
    integer i, j
    !res_xはx方向の残差,それがy本(jmin:jmax)ある
    !res_yはy方向の残差,それがx本(imin:imax)ある
    res_x(:, :) = 0.0d0
    res_y(:, :) = 0.0d0
    do j = jmin, jmax
        do i = imin, imax
            res_x(j, :) = res_x(j, :) + abs(bq(i, j, :) - bq_n(i, j, :)) / dble(imax - imin + 1)
            res_y(i, :) = res_y(i, :) + abs(bq(i, j, :) - bq_n(i, j, :)) / dble(jmax - jmin + 1)
        end do
    end do

    !do i = 0, imax
        !do j = 0, jmax
            !res_y(i, :) = res_y(i, :) + abs(bq(i, j, :) - bq_n(i, j, :)) / dble(jmax + 1)
        !end do
    !end do

end subroutine calc_res