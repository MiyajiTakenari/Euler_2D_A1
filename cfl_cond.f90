subroutine cflc
    use params
    use globals
    use interface_mod, only : bqtoq

    implicit none
    real(8) dif_time, temp_q(4), u1, u2, cmax_m, cmax_n, c_m, c_n, c, c_max
    integer i, j
    !dif_time: 前回のループで計算したbq(今持ってるbq)の時間と、欲しい解の時間(ex_time)の差
    dif_time = abs(ex_time - time)

    !↓今回のループで使うdtの計算
    !cmax_m, cmax_nの初期値
    temp_q(:) = bqtoq(bq(0, 0, :))
    !u = temp_q(2)
    !v = temp_q(3)
    u1 = mx(0, 0) * temp_q(2) + my(0, 0) * temp_q(3)
    u2 = nx(0, 0) * temp_q(2) + ny(0, 0) * temp_q(3)
    c = sqrt(gamma * temp_q(4) / temp_q(1))
    cmax_m = (abs(u1) + c * sqrt(mx(0, 0) ** 2.0d0 + my(0, 0) ** 2.0d0)) / s_j(0,0)
    cmax_n = (abs(u2) + c * sqrt(nx(0, 0) ** 2.0d0 + ny(0, 0) ** 2.0d0)) / s_j(0,0)

    !cmax_m, cmax_nを求める
    do i = 0, imax
        do j = 0, jmax
            temp_q(:) = bqtoq(bq(i, j, :))
            !u = temp_q(2)
            !v = temp_q(3)
            u1 = mx(i, j) * temp_q(2) + my(i, j) * temp_q(3)
            u2 = nx(i, j) * temp_q(2) + ny(i, j) * temp_q(3)
            c = sqrt(gamma * temp_q(4) / temp_q(1))
            c_m = (abs(u1) + c * sqrt(mx(i, j) ** 2.0d0 + my(i, j) ** 2.0d0)) / s_j(i,j)
            c_n = (abs(u2) + c * sqrt(nx(i, j) ** 2.0d0 + ny(i, j) ** 2.0d0)) / s_j(i,j)
            if (cmax_m < c_m) then
                cmax_m = c_m
            end if
            if (cmax_n < c_n) then
                cmax_n = c_n
            end if
        end do
    end do

    !max_m, cmax_nからdtを求める
    c_max = max(cmax_m, cmax_n)
    dt = cfl / c_max

    time = time + dt
    !今のdif_time > 前のdif_timeで計算終わる
    if (abs(ex_time - time) > dif_time) then
        write(*, *) "time =", time - dt
        exit_flag = 1
    endif
end subroutine cflc