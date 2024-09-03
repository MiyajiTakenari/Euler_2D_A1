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
    do n = 1, nmax
        call bound
        call cflc
        if (exit_flag == 1) exit
        call integ
        call calc_res
        call writed(n)
        !write (*, *) 'dt = ', dt
    end do
    write(*,'(a9, i3)') 'ntime = ', n

    call alloc(0)

end program euler_2D_A1