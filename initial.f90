subroutine init
    use params
    use globals
    use interface_mod, only : qtobq

    implicit none
    integer j
    !空間初期分布x
    do j = 1, jmax
        x(j) = -0.5d0 + dx * dble(j - 1)
    enddo

    !初期条件Q
    do j = -1, jmax+2
        if (-0.5d0 + dx * dble(j - 1) <= 0.0d0) then
            bq(j, 1:3) = qtobq(1.0d0, 0.0d0, 1.0d0)
        else
            bq(j, 1:3) = qtobq(0.125d0, 0.0d0, 0.1d0)
        endif
    enddo
end subroutine init