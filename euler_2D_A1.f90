program euler_2D_A1
    use params
    use globals
    use interface_mod

    implicit none
    integer :: n
    call alloc(1)
    call glid
    call init
    !時間進める
    do n = 1, nmax
        call bound
        call cflc
        if (exit_flag == 1) exit
        call xflux
        call nextq
        call calc_res
        call writed(n)
    end do
    call alloc(0)

    write(*, '(a9, i4)') "n_time =", n - 1
    write(*, *) "res_rho =", res(1)
    write(*, *) "res_u   =", res(2)
    write(*, *) "res_p   =", res(3)

end program euler_2D_A1