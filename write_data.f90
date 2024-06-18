subroutine writed(n)
    use params
    use globals
    use interface_mod, only : bqtoq

    implicit none
    integer, intent(in) :: n
    integer j
    real(8) temp_q(3)
    open(10, file = 'data_q.csv')
    do j = 0, jmax
        temp_q(:) = bqtoq(bq(j, 1:3))
        write(10, *) x(j), ',', temp_q(1), ',', temp_q(2), ',', temp_q(3)
    enddo
    close(10)

    open(11, file = 'data_tres.csv')
    write(11, '(a9, a1, f12.10, a1, a9, a1, i4)') "time =", ',', time, ',', "n_time =", ',', n
    write(11, '(a9, a1, a9, a1, a9, a1, a9)') "x", ',', "rho", ',', "u", ',', "p"
    write(11, '(a9, a1, e12.6, a1, e12.6, a1, e12.6)') "res", ',', res(1), ',', res(2), ',', res(3)
    close(11)
end subroutine writed