subroutine glid
    use params
    use globals

    implicit none
    integer i, j
    do i = imin-3, imax+2
        do j = jmin-3, jmax+2
            !物理空間初期分布
            x(i, j) = -0.5d0 + dx * dble(i)
            y(i, j) = -0.5d0 + dy * dble(j)
        end do
    end do

end subroutine glid