program euler_2D_A1
    use params
    use globals
    use interface_mod

    implicit none
    integer :: n
    call alloc(1)
    call glid
    call init
    call metrics
    !時間進める
    do n = 1, 1!nmax
        call bound
        call cflc
        if (exit_flag == 1) exit
        rec(n, 1) = time
        call integ
        rec(n, 2) = bq_n(50, 50, 1)
        call calc_res
        call writed(n)
    end do

    call alloc(0)

end program euler_2D_A1