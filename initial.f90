subroutine init
    use params
    use globals
    use interface_mod, only : qtobq, glid

    implicit none
    integer i, j

    !初期条件Q
    call glid
    do i = -2, imax+2
        do j = -2, jmax+2
            if (x(i, j) <= 0.0d0) then
                !-0.5d0 + dx * x(i, j) <= 0.0d0
                bq(i, j, 1:4) = qtobq(1.0d0, 0.0d0, 0.0d0, 1.0d0)
            else
                bq(i, j, 1:4) = qtobq(0.125d0, 0.0d0, 0.0d0, 0.1d0)
            endif
        enddo
    enddo

end subroutine init