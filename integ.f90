subroutine integ
    use params
    use globals
    use interface_mod, only : xflux, yflux

    implicit none
    real(8), allocatable :: bq_n(:, :, :)
    integer i, j

    allocate (bq_n(0:imax, 0:jmax, 4))

    !2段階2次精度スキーム
    bq_n(0:imax, 0:jmax, 1:4) = bq(0:imax, 0:jmax, 1:4)
    call xflux
    call yflux
    do i = 0, imax
        do j = 0, jmax
            bq(i, j, :) = bq(i, j, :) - (1.0d0 / s_j(i, j)) * (dt / 2.0d0) * ((e(i, j, :) - e(i-1, j, :)) + (f(i, j, :) - f(i, j-1, :)))
        end do
    end do

    call xflux
    call yflux
    do i = 0, imax
        do j = 0, jmax
            bq(i, j, :) = bq_n(i, j, :) - (1.0d0 / s_j(i, j)) * dt * ((e(i, j, :) - e(i-1, j, :)) + (f(i, j, :) - f(i, j-1, :)))
        end do
    end do

    deallocate (bq_n)

end subroutine integ