subroutine init
    use params
    use globals
    use interface_mod, only : qtobq

    implicit none
    integer i, j

    !初期条件Q
    !call glid
    do i = imin-4, imax+4
        do j = jmin-4, jmax+4
            if (x(i, j) <= 0.0d0) then
                !-0.5d0 + dx * x(i, j) <= 0.0d0
                bq(i, j, 1:4) = qtobq(1.0d0, 0.0d0, 0.0d0, 1.0d0)
            else
                bq(i, j, 1:4) = qtobq(0.125d0, 0.0d0, 0.0d0, 0.1d0)
            endif
        enddo
    enddo

    open(20,file = 'Qascii_00000.dat')
    !Qの内容を読み込みます
    rewind(20)
    write(20, *) 'meshfile.txt'
    do j= jmin-4, jmax+4
        do i= imin-4, imax+4
            write(20, *) bq(i,j,1), bq(i,j,2), bq(i,j,3), bq(i,j,4)
        enddo
    enddo
    close(20)

end subroutine init