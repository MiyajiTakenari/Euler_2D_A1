subroutine calc_res
    use params
    use globals

    implicit none
    integer j
    res(1:3) = 0.0d0
    do j = 1, jmax
        res(:) = res(:) + abs(-(dt / dx) * (e(j, :) - e(j-1, :))) / jmax
    enddo
end subroutine calc_res