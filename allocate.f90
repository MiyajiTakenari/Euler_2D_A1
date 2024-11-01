subroutine alloc(al_flag)
    use params
    use globals

    implicit none
    integer, intent(in) :: al_flag
    if (al_flag == 1) then
        !表示するqつまりQはmin~max
        allocate (x(imin-5:imax+4, jmin-5:jmax+4), y(imin-5:imax+4, jmin-5:jmax+4))
        allocate (bq(imin-4:imax+4, jmin-4:jmax+4, 4))
        allocate (e(imin-3:imax+2, jmin-3:jmax+2, 4))
        allocate (f(imin-3:imax+2, jmin-3:jmax+2, 4))
        allocate (mx(imin-5:imax+4, jmin-4:jmax+4), my(imin-5:imax+4, jmin-4:jmax+4))
        allocate (nx(imin-4:imax+4, jmin-5:jmax+4), ny(imin-4:imax+4, jmin-5:jmax+4))
        allocate (s_j(imin-2:imax+2, jmin-2:jmax+2))
        allocate (bq_n(imin:imax, jmin:jmax, 4))
        allocate (res_x(jmin:jmax, 4), res_y(imin:imax, 4))
        !B.Cのfuncion ave_n, ave_mでmetricsの配列の下限値使ってる。下限値変更時注意
    else if (al_flag == 0) then
        deallocate (x, y, bq, e, f, mx, my, nx, ny, s_j, bq_n, res_x, res_y)
    end if

end subroutine alloc