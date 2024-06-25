subroutine alloc(al_flag)
    use params
    use globals

    implicit none
    integer, intent(in) :: al_flag
    if (al_flag == 1) then
        allocate (x(-4:imax+2, -4:jmax+2), y(-4:imax+2, -4:jmax+2))
        allocate (bq(-2:imax+2, -2:jmax+2, 4))
        allocate (e(-1:imax, -1:jmax, 4))
        allocate (f(-1:imax, -1:jmax, 4))
        allocate (mx(-3:imax+2, -3:jmax+2), my(-3:imax+2, -3:jmax+2))
        allocate (nx(-3:imax+2, -3:jmax+2), ny(-3:imax+2, -3:jmax+2))
        allocate (s_j(0:imax, 0:jmax))
        allocate (res_x(0:jmax, 4), res_y(0:imax, 4))
        !B.Cのfuncion ave_n, ave_mでmetricsの配列の下限値使ってる。下限値変更時注意
    else if (al_flag == 0) then
        deallocate (x, y, bq, e, f, mx, my, nx, ny, s_j, res_x, res_y)
    end if

end subroutine alloc