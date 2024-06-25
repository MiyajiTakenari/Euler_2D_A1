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
        & , ',', e(i, j, 1) , ',', rec(i+1, 1), ',', rec(i+1, 2), ',', rec(i+1, 3), ',', rec(i+1, 4), ',', rec(i+1, 5)
    enddo
    close(10)

    open(11, file = 'data_tres.csv')
    write(11, '(a9, a1, f12.10, a1, a9, a1, i4)') "time =", ',', time, ',', "n_time =", ',', n
    write(11, '(a9, a1, a9, a1, a9, a1, a9, a1, a9)') "x", ',', "rho", ',', "rho*u", ',', "rho*v", ',', "e"
    write(11, '(a9, a1, e12.6, a1, e12.6, a1, e12.6, a1, e12.6)') &
    & "res", ',', res_x(j, 1), ',', res_x(j, 2), ',', res_x(j, 3), ',', res_x(j, 4)
    close(11)
end subroutine writed