subroutine glid
    use params
    use globals

    implicit none
    integer i, j

    open(13,file = 'meshfile.txt')
    do j = jmin-3, jmax+2
        do i = imin-3, imax+2
            !物理空間初期分布
            x(i, j) = -range_x / 2.0d0 + dx * dble(i)
            y(i, j) = -range_y / 2.0d0 + dy * dble(j)
            write(13,*) x(i,j), ',',  y(i,j)
        end do
    end do
    close(13)

end subroutine glid