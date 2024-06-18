subroutine calc_res
    use params
    use globals

    implicit none
    integer j
    res(1:3) = 0.0d0
    do j = 0, jmax
        res(:) = res(:) + abs(-(dt / dx) * (e(j, :) - e(j-1, :))) / (jmax+1)
        !xの配列数で割る
    enddo
end subroutine calc_res