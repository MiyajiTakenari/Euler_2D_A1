subroutine xflux
    use params
    use globals
    use interface_mod, only : bqtoq, delbar

    implicit none
    integer j
    real(8) ql(3), qr(3), bar_m(3), bar_p(3), & !MUSCLで使う
    & rho_l, rho_r, u_l, u_r, p_l, p_r, & !MUSCLの答え
    & a_l, a_r, cm, ul_p, ur_m, pl_p, pr_m, & !2,4,3で使う
    & rus_r, rus_d, mdot, s, & !5で使う
    & e_l, e_r, h_l, h_r !Flux評価で使う

    !位置進めるE(-1:jmax)
    !fluxの計算
    do j = -1, jmax
        !MUSCL approach
        !qi = bqtoq(bq(j,3))
        !qlを計算
        call delbar(bqtoq(bq(j-1,:)), bqtoq(bq(j,:)), bqtoq(bq(j+1,:)), bar_p, bar_m)
        ql(:) = bqtoq(bq(j,:)) + 0.25d0 * (1.0d0 - phi) * bar_m(:) + 0.25d0 * (1.0d0 + phi) * bar_p(:)
        rho_l = ql(1)
        u_l = ql(2)
        p_l = ql(3)
        !qrを計算
        call delbar(bqtoq(bq(j,:)), bqtoq(bq(j+1,:)), bqtoq(bq(j+2,:)), bar_p, bar_m)
        qr(:) = bqtoq(bq(j+1,:)) - 0.25d0 * (1.0d0 - phi) * bar_p(:) - 0.25d0 * (1.0d0 + phi) * bar_m(:)
        rho_r= qr(1)
        u_r = qr(2)
        p_r = qr(3)

        !2,4,3を計算
        a_l = (2.0d0 * (p_l / rho_l)) / ((p_l/rho_l) + (p_r/rho_r))
        a_r = (2.0d0 * (p_r / rho_r)) / ((p_l/rho_l) + (p_r/rho_r))
        cm = max(sqrt(gamma * p_l / rho_l), sqrt(gamma * p_r / rho_r))

        if (abs(u_l)/cm <= 1.0d0) then
            ul_p = ((a_l * (u_l + cm) ** 2.0d0) / (4.0d0 * cm)) + ((1.0d0 - a_l) * (u_l + abs(u_l)) / 2.0d0)
            pl_p = (p_l * (2.0d0 - (u_l/cm)) * ((u_l/cm) + 1.0d0) ** 2.0d0) / 4.0d0
        else
            ul_p = (u_l + abs(u_l)) / 2.0d0
            pl_p = p_l * (u_l + abs(u_l)) / (2.0d0 * u_l)
        endif

        if (abs(u_r)/cm <= 1.0d0) then
            ur_m = ((-a_r * (u_r - cm) ** 2.0d0) / (4.0d0 * cm)) + ((1.0d0 - a_r) * (u_r - abs(u_r)) / 2.0d0)
            pr_m = (p_r * (2.0d0 + (u_r/cm)) * ((u_r/cm) - 1.0d0) ** 2.0d0) / 4.0d0
        else
            ur_m = (u_r - abs(u_r)) / 2.0d0
            pr_m = p_r * (u_r - abs(u_r)) / (2.0d0 * u_r)
        endif

        !5,1を計算
        rus_r = ul_p * (rho_l * u_l) + ur_m * (rho_r * u_r)
        mdot = ul_p * rho_l + ur_m * rho_r
        rus_d = 0.5d0 * (mdot * (u_l + u_r) - abs(mdot) * (u_r - u_l))
        s = 0.5d0 * min(1.0d0, 10.0d0 * abs(p_r - p_l) / min(p_l, p_r))

        !Eを計算
        e(j,1) = mdot
        e(j,2) = (0.5d0 + s) * rus_r + (0.5d0 - s) * rus_d + pl_p + pr_m
        e_l = (p_l / (gamma - 1.0d0)) + 0.5d0 * rho_l * u_l ** 2.0d0
        e_r = (p_r / (gamma - 1.0d0)) + 0.5d0 * rho_r * u_r ** 2.0d0
        h_l = (e_l + p_l) / rho_l
        h_r = (e_r + p_r) / rho_r
        e(j,3) = 0.5d0 * (mdot * (h_l + h_r) - abs(mdot) * (h_r - h_l))

    enddo
end subroutine xflux