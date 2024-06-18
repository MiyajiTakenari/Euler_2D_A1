subroutine cflc
    use params
    use globals
    use interface_mod, only : bqtoq

    implicit none
    real(8) dif_time, q_max, temp_q(3)
    integer j
    !dt計算
    !dif_time: 前回のループで計算したbq(今持ってるbq)の時間と、欲しい解の時間(ex_time)の差
    dif_time = abs(ex_time - time)
    !↓今回のループで使うdtの計算
    q_max = -1.0d0
    do j = 0, jmax
        temp_q(:) = bqtoq(bq(j, 1:3))
        if ( q_max < abs(temp_q(2)) + abs(sqrt(gamma * temp_q(3) / temp_q(1))) ) then
            q_max = abs(temp_q(2)) + abs(sqrt(gamma * temp_q(3) / temp_q(1)))
        endif
    enddo
    dt = cfl * dx / q_max
    time = time + dt
    !今のdif_time > 前のdif_timeで計算終わる
    if (abs(ex_time - time) > dif_time) then
        write(*, *) "time =", time - dt
        exit_flag = 1
    endif
end subroutine cflc