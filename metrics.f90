subroutine metrics
    use globals
    use params

    implicit none
    integer i, j
    do i = -3, imax+2
        do j = -3, jmax+2
            mx(i, j) = (y(i, j) - y(i, j-1))
            my(i, j) = -(x(i, j) - x(i, j-1))
            nx(i, j) = -(y(i, j) - y(i-1, j))
            ny(i, j) = (x(i, j) - x(i-1, j))
        end do
    end do

    do i = 0, imax
        do j = 0, jmax
            s_j(i, j) = ny(i, j) * mx(i, j) - nx(i, j) * my(i, j)
        end do
    end do
end subroutine metrics