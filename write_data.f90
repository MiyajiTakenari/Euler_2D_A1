subroutine writed(n)
    use params
    use globals
    use interface_mod, only : bqtoq

    implicit none
    integer, intent(in) :: n
    integer i, j
    real(8) x_write, y_write, temp_q(4)
    open(10, file = 'data_q.csv')
    j = 10
    do i = imin, imax
        !q(cell center)を表示
        temp_q(:) = bqtoq(bq(i, j, :))
        !glid(cell vertex)をq(i,j)の位置(cell center)にするため、cell center周りの4隅を平均する
        x_write = (x(i-1, j-1) + x(i, j-1) + x(i, j) + x(i-1, j)) / 4.0d0
        y_write = (y(i-1, j-1) + y(i, j-1) + y(i, j) + y(i-1, j)) / 4.0d0
        write(10, *) x_write, ',', temp_q(1), ',', temp_q(2), ',', temp_q(3), ',', temp_q(4)
        !& , ',', ',', e(i, j, 1), ',', e(i, j, 2), ',', e(i, j, 3), ',', e(i, j, 4) &
        !& , ',', ',', f(j, i, 1), ',', f(j, i, 2), ',', f(j, i, 3), ',', f(j, i, 4) &
        !& , ',', ',', rec(i, 1), ',', rec(i, 2), ',', rec(i, 3), ',', rec(i, 4), ',', rec(i, 5)
    enddo
    close(10)
    ! x4   x3     q(i,j) = cell_center
    !    q        x1(i-1, j-1), x2(i, j-1)
    ! x1   x2     x3(i, j), x4(i-1, j)

    open(11, file = 'data_tres.csv')
    write(11, '(a9, a1, f12.10, a1, a9, a1, i4)') "time =", ',', time, ',', "n_time =", ',', n
    write(11, '(a9, a1, a9, a1, a9, a1, a9, a1, a9)') "x", ',', "rho", ',', "rho*u", ',', "rho*v", ',', "e"
    write(11, '(a9, a1, e12.6, a1, e12.6, a1, e12.6, a1, e12.6)') &
    & "res", ',', res_x(j, 1), ',', res_x(j, 2), ',', res_x(j, 3), ',', res_x(j, 4)
    close(11)
end subroutine writed