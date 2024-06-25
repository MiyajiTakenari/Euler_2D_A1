subroutine writed(n)
    use params
    use globals
    use interface_mod, only : bqtoq

    implicit none
    integer, intent(in) :: n
    integer i, j
    real(8) temp_q(4)
    open(10, file = 'data_q.csv')
    j = 50
    do i = 0, imax
        temp_q(:) = bqtoq(bq(i, j, :))
        write(10, *) x(i, j), ',', temp_q(1), ',', temp_q(2), ',', temp_q(3), ',', temp_q(4) &
        & , ',', ',', e(i, j, 1), ',', e(i, j, 2), ',', e(i, j, 3), ',', e(i, j, 4) &
        & , ',', ',', f(j, i, 1), ',', f(j, i, 2), ',', f(j, i, 3), ',', f(j, i, 4) &
        & , ',', ',', rec(i, 1), ',', rec(i, 2), ',', rec(i, 3), ',', rec(i, 4), ',', rec(i, 5) &
        & , ',', rec(i, 6), ',', rec(i, 7), ',', rec(i, 8), ',', rec(i, 9), ',', rec(i, 10) &
        & , ',', rec(i, 11), ',', rec(i, 12), ',', rec(i, 13) &
        & , ',', ',', rec(i, 21), ',', rec(i, 22), ',', rec(i, 23), ',', rec(i, 24), ',', rec(i, 25) &
        & , ',', rec(i, 26), ',', rec(i, 27), ',', rec(i, 28), ',', rec(i, 29), ',', rec(i, 30) &
        & , ',', rec(i, 31), ',', rec(i, 32), ',', rec(i, 33)
    enddo
    close(10)

    open(11, file = 'data_tres.csv')
    write(11, '(a9, a1, f12.10, a1, a9, a1, i4)') "time =", ',', time, ',', "n_time =", ',', n
    write(11, '(a9, a1, a9, a1, a9, a1, a9, a1, a9)') "x", ',', "rho", ',', "rho*u", ',', "rho*v", ',', "e"
    write(11, '(a9, a1, e12.6, a1, e12.6, a1, e12.6, a1, e12.6)') &
    & "res", ',', res_x(j, 1), ',', res_x(j, 2), ',', res_x(j, 3), ',', res_x(j, 4)
    close(11)
end subroutine writed