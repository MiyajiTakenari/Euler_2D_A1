subroutine alloc(al_flag)
    use params
    use globals

    implicit none
    integer, intent(in) :: al_flag
    if (al_flag == 1) then
        allocate (x(-4:imax+2, -4:jmax+2), y(-4:imax+2, -4:jmax+2))
        allocate (bq(-2:imax+2, -2:jmax+2, 4))
        allocate (e(-1:imax, -1:jmax, 4))
        allocate (mx(-3:imax+2, -3:jmax+2), my(-3:imax+2, -3:jmax+2))
        allocate (nx(-3:imax+2, -3:jmax+2), ny(-3:imax+2, -3:jmax+2))
        !B.Cのfuncion ave_n, ave_mでmetricsの配列の下限値使ってる。下限値変更時注意
        allocate (s_j(0:imax, 0:jmax))
    else if (al_flag == 0) then
        deallocate (x, y, bq, e, mx, my, nx, ny, s_j)
    end if

end subroutine alloc