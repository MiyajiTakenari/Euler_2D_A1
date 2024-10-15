subroutine metrics
    use globals
    use params

    implicit none
    integer i, j

    do i = imin-5, imax+4
        do j = jmin-4, jmax+4
            mx(i, j) = (y(i, j) - y(i, j-1))
            my(i, j) = -(x(i, j) - x(i, j-1))
        end do
    end do

    do j = jmin-5, jmax+4
        do i = imin-4, imax+4
            nx(i, j) = -(y(i, j) - y(i-1, j))
            ny(i, j) = (x(i, j) - x(i-1, j))
        end do
    end do

    do i = imin-2, imax+2
        do j = jmin-2, jmax+2
            s_j(i, j) = ny(i, j) * mx(i, j) - nx(i, j) * my(i, j)
        end do
    end do
end subroutine metrics