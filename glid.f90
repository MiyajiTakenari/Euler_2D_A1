subroutine glid
    use params
    use globals

    implicit none
    integer i, j
    allocate(x(0:imax, 0:jmax), y(0:imax, 0:jmax))
    do i = 0, imax
        do j = 0, jmax
            !x(i, j) = dble(i)
            !y(i, j) = dble(j)
            x(i, j) = -0.5d0 + dx * dble(i - 1)
            y(i, j) = -0.5d0 + dy * dble(i - 1)
        end do
    end do

end subroutine glid