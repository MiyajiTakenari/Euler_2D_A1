subroutine glid
    use params
    use globals

    implicit none
    integer i, j
    do i = -4, imax+2
        do j = -4, jmax+2
            !x(i, j) = dble(i)
            !y(i, j) = dble(j)
            !物理空間初期分布
            x(i, j) = -0.5d0 + dx * dble(i)
            y(i, j) = -0.5d0 + dy * dble(j)
        end do
    end do

end subroutine glid